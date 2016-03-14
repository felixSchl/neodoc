module Language.Docopt.Parser.Desc where

import Prelude
import Debug.Trace
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Monad.State (get)
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Data.List (List(..), some, (:), toList, length
                 , singleton, many, head, catMaybes, filter)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.Foldable (intercalate)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe, maybe', isJust)
import Data.Generic
import Data.String (toLower, fromChar)
import qualified Data.Array as A
import qualified Data.String as Str

import Language.Docopt.Value
import Language.Docopt.Parser.Base
import Language.Docopt.Parser.Common
import Language.Docopt.Parser.State
import Language.Docopt.Parser.Lexer (lex)
import qualified Language.Docopt.Parser.Lexer as L

data Desc = OptionDesc Option
          | CommandDesc

data Name = Flag Char | Long String | Full Char String

getFlag :: Name -> Maybe Char
getFlag (Flag c)   = pure c
getFlag (Full c _) = pure c
getFlag _          = Nothing

getName :: Name -> Maybe String
getName (Long   n) = pure n
getName (Full _ n) = pure n
getName _          = Nothing

newtype Argument = Argument {
  name    :: String
, default :: Maybe Value
}

runArgument :: Argument -> {
  name    :: String
, default :: Maybe Value
}
runArgument (Argument a) = a

newtype Option = Option {
  name :: Name
, arg  :: Maybe Argument
, env  :: Maybe String
}

data Content
  = Text
  | Default String
  | Env     String

isDefaultTag :: Content -> Boolean
isDefaultTag (Default _) = true
isDefaultTag _           = false

getDefaultValue :: Content -> Maybe String
getDefaultValue (Default v) = Just v
getDefaultValue _           = Nothing

isEnvTag :: Content -> Boolean
isEnvTag (Env _) = true
isEnvTag _       = false

getEnvKey :: Content -> Maybe String
getEnvKey (Env k) = Just k
getEnvKey _       = Nothing

derive instance genericDesc     :: Generic Desc
derive instance genericArgument :: Generic Argument
derive instance genericName     :: Generic Name
derive instance genericOption   :: Generic Option
derive instance genericContent  :: Generic Content

instance showDesc :: Show Desc
  where show = gShow

instance showOption :: Show Option
  where show = gShow

instance showArgument :: Show Argument
  where show = gShow

instance showName :: Show Name
  where show = gShow

instance showContent :: Show Content
  where show = gShow

instance eqOption :: Eq Option
  where eq = gEq

instance eqArgument :: Eq Argument
  where eq = gEq

instance eqName :: Eq Name
  where eq = gEq

instance eqDesc :: Eq Desc
  where eq = gEq

prettyPrintDesc :: Desc -> String
prettyPrintDesc (OptionDesc opt) = "Option " ++ prettyPrintOption opt

prettyPrintOption :: Option -> String
prettyPrintOption (Option opt)
  = (name opt.name) ++ arg ++ env
  where
      name (Flag c)   = "-" ++ fromChar c
      name (Long n)   = "--" ++ n
      name (Full c n) = "-" ++ fromChar c ++ ", --" ++ n

      arg = maybe "" id do
        (Argument a) <- opt.arg
        return $ "=" ++ a.name ++ case a.default of
                                    Nothing  -> ""
                                    (Just v) -> " [default: "
                                                    ++ prettyPrintValue v
                                                    ++ "]"

      env = maybe "" id do
        k <- opt.env
        return $ " [env: " ++ k ++ "]"

prettyPrintArgument :: Argument -> String
prettyPrintArgument (Argument { name: n, default: d })
  = n ++ maybe "" (\v -> " [default: " ++ (prettyPrintValue v) ++  "]") d

argument :: String -> Maybe String -> Argument
argument name default = Argument { name:    name
                                 , default: StringValue <$> default
                                 }

run :: String -> Either P.ParseError (List Desc)
run x = lex x >>= parse

parse :: (List L.PositionedToken) -> Either P.ParseError (List Desc)
parse = flip L.runTokenParser descParser

descParser :: L.TokenParser (List Desc)
descParser =
  markIndent do
    opt  <- option
    opts <- many $ sameIndent *> option
    return (opt : opts)
  where

    anyName :: L.TokenParser String
    anyName = L.angleName <|> L.shoutName <|> L.name

    option :: L.TokenParser Desc
    option = do
      xopt@(Option opt) <- start

      description <- do
        flip P.manyTill (L.eof <|> (P.lookAhead $ P.try $ void start)) do
          P.choice $ P.try <$> [
            Default <$> L.tag "default"
          , Env     <$> L.tag "env"
          , L.anyToken *> pure Text
          ]


      let defaults = getDefaultValue <$> filter isDefaultTag description
          envs     = getEnvKey       <$> filter isEnvTag     description

      if (length defaults > 1)
         then P.fail $
          "Option " ++ (show $ prettyPrintOption xopt)
                    ++ " has multiple defaults!"
         else return unit

      if (length envs > 1)
         then P.fail $
          "Option " ++ (show $ prettyPrintOption xopt)
                    ++ " has multiple environment mappings!"
         else return unit

      let default = head defaults >>= id
          env     = head envs     >>= id

      if (isJust default) && (not $ isJust opt.arg)
         then P.fail $
          "Option " ++ (show $ prettyPrintOption xopt)
                    ++ " does not take arguments. "
                    ++ "Cannot specify defaults."
         else return unit

      return $ OptionDesc $ Option $
        opt { env = env
            , arg = do
                (Argument arg) <- opt.arg
                return $ Argument $ arg {
                  default = StringValue <$> default
                }
            }

      where

        start :: L.TokenParser Option
        start = do
          P.choice $ P.try <$> [
            short <* do
              P.notFollowedBy do
                P.choice $ P.try <$> [ L.comma *> long, long ]
          , long <* do
              P.notFollowedBy do
                P.choice $ P.try <$> [ L.comma *> short, short ]
          , both
          ]

        short :: L.TokenParser Option
        short = do
          opt <- sopt

          arg <- maybe'
                  (\_ -> do P.optionMaybe (L.shoutName <|> L.angleName))
                  (return <<< Just)
                  opt.arg

          return $ Option { name: Flag opt.flag
                          , arg:  flip argument Nothing <$> arg
                          , env:  Nothing
                          }

        long :: L.TokenParser Option
        long = do
          opt <- lopt

          arg <- maybe'
                  (\_ -> do P.optionMaybe (L.shoutName <|> L.angleName))
                  (return <<< Just)
                  opt.arg

          return $ Option { name: Long opt.name
                          , arg:  flip argument Nothing <$>  opt.arg
                          , env:  Nothing
                          }

        both :: L.TokenParser Option
        both = markLine do
          x <- short
          y <- do sameLine
                  P.choice $ P.try <$> [ L.comma *> long
                                       , long
                                       ]
          combine x y

          where
            -- Combine two options into one. This function *does not* cover all
            -- cases right now. It deals only with a known subset and can there-
            -- fore make assumptions
            combine :: Option -> Option -> L.TokenParser Option
            combine (Option x@{ name: Flag f }) (Option y@{ name: Long n }) = do
              either P.fail return do
                arg <- combineArg x.arg y.arg
                return $ Option {
                  name: Full f n
                , arg: arg
                , env: Nothing -- No need to keep at this stage
                }

              where
                combineArg (Just a) (Just b) | (a == b) = return (pure a)
                combineArg Nothing  (Just b)            = return (pure b)
                combineArg (Just a) Nothing             = return (pure a)
                combineArg Nothing Nothing              = return Nothing
                combineArg (Just a) (Just b) | (a /= b) = Left $
                        "Arguments mismatch: " ++ (show $ prettyPrintArgument a)
                                    ++ " and " ++ (show $ prettyPrintArgument b)

    sopt :: L.TokenParser { flag :: Char, arg :: Maybe String }
    sopt = do
      opt <- L.sopt
      (guard $ (A.length opt.stack == 0)) P.<?> "No stacked options"
      return { flag: opt.flag, arg: opt.arg }

    lopt :: L.TokenParser { name :: String, arg :: Maybe String }
    lopt = L.lopt
