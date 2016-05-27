module Language.Docopt.Parser.Desc (
    Desc (..)
  , Name (..)
  , OptionObj ()
  , OptionArgumentObj ()
  , getFlag
  , getName
  , prettyPrintDesc
  , run
  , parse
  ) where

import Prelude
import Data.Tuple (Tuple (Tuple))
import Data.Functor (($>))
import Data.Function (on)
import Control.Lazy (defer)
import Control.Bind ((>=>))
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.MonadPlus (guard)
import Data.List (List, (:), many, some, head, length, filter, catMaybes)
import Text.Parsing.Parser (ParseError, fail) as P
import Text.Parsing.Parser.Combinators ((<?>), try, choice, lookAhead, manyTill,
                                        option, optionMaybe, notFollowedBy,
                                        (<??>)) as P
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Nothing, Just), isJust, isNothing, maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (fromChar)
import Data.Array as A
import Data.String.Ext ((^=))

import Language.Docopt.Value (Value, prettyPrintValue)
import Language.Docopt.Parser.Common (sameIndent, markIndent, indented,
                                     moreIndented, lessIndented)
import Language.Docopt.Parser.Lexer (lexDescs)
import Language.Docopt.Parser.Lexer as L
import Language.Docopt.Value as Value

data Desc
  = OptionDesc OptionObj
  | CommandDesc

data Name
  = Flag Char
  | Long String
  | Full Char String

type OptionObj = {
  name       :: Name
, arg        :: Maybe OptionArgumentObj
, env        :: Maybe String
, repeatable :: Boolean
}

showOptionObj :: OptionObj -> String
showOptionObj o = "{ name: "       <> show o.name
               <> ", arg: "        <> show (OptionArgument <$> o.arg)
               <> ", env: "        <> show o.env
               <> ", repeatable: " <> show o.repeatable
               <> "}"

eqOptionObj :: OptionObj -> OptionObj -> Boolean
eqOptionObj o o' = o.name                     == o'.name
                && (OptionArgument <$> o.arg) == (OptionArgument <$> o'.arg)
                && o.env                      == o'.env
                && o.repeatable               == o'.repeatable

type OptionArgumentObj = {
  name     :: String
, default  :: Maybe Value
, optional :: Boolean
}

showOptionArgumentObj :: OptionArgumentObj -> String
showOptionArgumentObj o = "{ name: "     <> show o.name
                       <> ", default: "  <> show o.default
                       <> ", optional: " <> show o.optional
                       <> "}"

eqOptionArgumentObj :: OptionArgumentObj -> OptionArgumentObj -> Boolean
eqOptionArgumentObj a a' = a.name     == a'.name
                        && a.default  == a'.default
                        && a.optional == a'.optional

newtype OptionArgument = OptionArgument OptionArgumentObj

unOptionArgument :: OptionArgument -> OptionArgumentObj
unOptionArgument (OptionArgument a) = a

instance showOptionArgument :: Show OptionArgument where
  show = showOptionArgumentObj <<< unOptionArgument

instance eqOptionArgument :: Eq OptionArgument where
  eq = eqOptionArgumentObj `on` unOptionArgument

getFlag :: Name -> Maybe Char
getFlag (Flag f)   = pure f
getFlag (Full f _) = pure f
getFlag _          = Nothing

getName :: Name -> Maybe String
getName (Long   n) = pure n
getName (Full _ n) = pure n
getName _          = Nothing

data Content
  = Default String
  | Env     String

isDefaultTag :: Content -> Boolean
isDefaultTag (Default _) = true
isDefaultTag _           = false

getDefaultValue :: Content -> Maybe Value
getDefaultValue (Default v) = either (const Nothing) Just (Value.parse v true)
getDefaultValue _           = Nothing

isEnvTag :: Content -> Boolean
isEnvTag (Env _) = true
isEnvTag _       = false

getEnvKey :: Content -> Maybe String
getEnvKey (Env k) = Just k
getEnvKey _       = Nothing

derive instance genericName    :: Generic Name
derive instance genericContent :: Generic Content

instance showName     :: Show Name     where show = gShow
instance showContent  :: Show Content  where show = gShow
instance eqName       :: Eq Name       where eq = gEq

prettyPrintDesc :: Desc -> String
prettyPrintDesc (OptionDesc opt) = "Option " <> prettyPrintOption opt
prettyPrintDesc (CommandDesc) = "Command"

instance showDesc :: Show Desc where
  show (OptionDesc o) = "OptionDesc " <> showOptionObj o
  show (CommandDesc)  = "CommandDesc"

instance eqDesc :: Eq Desc where
  eq (OptionDesc o) (OptionDesc o') = eqOptionObj o o'
  eq (CommandDesc) (CommandDesc)    = true
  eq _             _                = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption opt
  = (name opt.name) <> arg <> env
  where
      name (Flag c)   = "-"  <> fromChar c
      name (Long n)   = "--" <> n
      name (Full c n) = "-"  <> fromChar c <> ", --" <> n

      arg = maybe "" id do
        a <- opt.arg
        pure $
          (if a.optional then "[" else "")
            <> "=" <> a.name
            <> (if a.optional then "]" else "")
            <> (if opt.repeatable then "..." else "")
            <> (maybe ""
                      (\v -> "[default: " <> prettyPrintValue v <> "]")
                      a.default)

      env = maybe "" id do
        k <- opt.env
        pure $ " [env: " <> k <> "]"

prettyPrintOptionArgument :: OptionArgumentObj -> String
prettyPrintOptionArgument { optional: o, name: n, default: d }
  = (if o then "[" else "") <> n <> (if o then "]" else "")
    <> maybe "" (\v -> " [default: " <> (prettyPrintValue v) <>  "]") d

run :: String -> Either P.ParseError (List Desc)
run = lexDescs >=> parse

parse :: (List L.PositionedToken) -> Either P.ParseError (List Desc)
parse = flip L.runTokenParser descParser

descParser :: L.TokenParser (List Desc)
descParser = markIndent do many desc <* L.eof
  where

    anyName :: L.TokenParser String
    anyName = L.angleName <|> L.shoutName <|> L.name

    desc :: L.TokenParser Desc
    desc = defer \_-> "--option or <positional> description" P.<??> do
            P.choice $ [ optionDesc
                       , positionalsDesc
                       ]

    descContent :: L.TokenParser (List Content)
    descContent = do
      markIndent do
        catMaybes <$> (flip P.manyTill descEnd do
          P.choice $ P.try <$> [
            Just <<< Default <$> L.tag "default"
          , Just <<< Env     <$> L.tag "env"
          , L.anyToken $> Nothing
          ])
      <* (void L.eof <|> void (some L.newline))
      where
        descEnd = do
          P.choice [
            L.eof
          , void $ P.lookAhead do
              L.newline
              lessIndented
              P.choice [
                void L.sopt
              , void L.lopt
              , void L.angleName
              , void L.shoutName
              ]
          ]

    positionalsDesc :: L.TokenParser Desc
    positionalsDesc = do
      L.angleName <|> L.shoutName
      repeatable <- P.option false $ L.tripleDot $> true
      descContent
      pure CommandDesc

    optionDesc :: L.TokenParser Desc
    optionDesc = do

      xopt        <- opt
      description <- descContent

      let defaults = getDefaultValue <$> filter isDefaultTag description
          envs     = getEnvKey       <$> filter isEnvTag     description

      if (length defaults > 1)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " has multiple defaults!"
         else pure unit

      if (length envs > 1)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " has multiple environment mappings!"
         else pure unit

      let default = head defaults >>= id
          env     = head envs     >>= id

      if (isJust default) && (isNothing xopt.arg)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " does not take arguments. "
                    <> "Cannot specify defaults."
         else pure unit

      pure $ OptionDesc $
        xopt { env = env
            , arg = do
                arg <- xopt.arg
                pure $ arg {
                  default = default
                }
            }

      where

        short :: L.TokenParser OptionObj
        short = do
          opt <- do
            opt <- L.sopt
            (guard $ (A.length opt.stack == 0)) P.<?> "No stacked options"
            pure { flag: opt.flag, arg: opt.arg }

          -- Grab the adjacent positional-looking argument
          -- in case the token did not have an explicit
          -- binding via `=`.
          arg <- maybe
                  (P.optionMaybe do
                    n <- L.shoutName <|> L.angleName
                    pure { name: n, optional: false }
                  )
                  (pure <<< Just)
                  opt.arg

          repeatable <- P.option false $ L.tripleDot $> true

          pure $ { name: Flag opt.flag
                   , arg:  do
                       a <- arg
                       pure {
                         name:     a.name
                       , optional: a.optional
                       , default:  Nothing
                       }
                   , env:        Nothing
                   , repeatable: repeatable
                   }

        long :: L.TokenParser OptionObj
        long = do
          opt <- L.lopt

          -- Grab the adjacent positional-looking argument
          -- in case the token did not have an explicit
          -- binding via `=`.
          arg <- maybe
                  (P.optionMaybe do
                    n <- L.shoutName <|> L.angleName
                    pure { name: n, optional: false }
                  )
                  (pure <<< Just)
                  opt.arg

          repeatable <- P.option false $ L.tripleDot $> true

          pure $ { name: Long opt.name
                    , arg:  do
                        a <- arg
                        pure {
                          name:     a.name
                        , optional: a.optional
                        , default:  Nothing
                        }
                    , env:        Nothing
                    , repeatable: repeatable
                    }

        opt :: L.TokenParser OptionObj
        opt = do
          x <- P.optionMaybe short
          y <- P.optionMaybe do
            P.choice $ P.try <$> do
              maybe [ long ]
                    (const [ L.comma  *> many L.newline *> indented *> long
                           , long
                           ])
                    x

          case Tuple x y of
            Tuple (Just x) (Just y) -> combine x y
            Tuple (Just x) Nothing  -> pure x
            Tuple Nothing  (Just y) -> pure y
            otherwise               -> P.fail "Expected options"

          where
            -- Combine two options into one. This function *does not* cover all
            -- cases right now. It deals only with a known subset and can there-
            -- fore make assumptions
            combine :: OptionObj -> OptionObj -> L.TokenParser OptionObj
            combine (x@{ name: Flag f }) (y@{ name: Long n }) = do
              either P.fail pure do
                arg <- combineArg x.arg y.arg
                pure $ {
                  name:       Full f n
                , arg:        arg
                , env:        Nothing -- No need to keep at this stage
                , repeatable: x.repeatable || y.repeatable
                }
              where
                combineArg (Just a) (Just a')
                  | (a.name ^= a'.name) = pure $ Just
                      $ { name:     a.name
                        , optional: a.optional || a'.optional
                        , default:  a.default <|> a'.default
                        }
                combineArg Nothing  (Just b) = pure (pure b)
                combineArg (Just a) Nothing  = pure (pure a)
                combineArg Nothing Nothing   = pure Nothing
                combineArg (Just a) (Just b) = Left $
                        "Option-arguments mismatch: "
                          <> (show $ prettyPrintOptionArgument a)
                          <> " and "
                          <> (show $ prettyPrintOptionArgument b)
            combine _ _ = P.fail "Invalid case - expected flag and long option"
