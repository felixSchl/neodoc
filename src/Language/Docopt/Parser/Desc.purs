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
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Generic
import Data.String (toLower, fromChar)
import qualified Data.Array as A
import qualified Data.String as Str

import Language.Docopt.Parser.Base
import Language.Docopt.Parser.Common
import Language.Docopt.Parser.State
import Language.Docopt.Parser.Lexer (lex)
import qualified Language.Docopt.Parser.Lexer as L

data Desc = OptionDesc Option
          | CommandDesc

data Name        = Flag Char | Long String | Full Char String
newtype Argument = Argument { name :: String, default :: Maybe String }
newtype Option   = Option   { name :: Name, arg :: Maybe Argument }
data Content     = Text | Default String

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
  = name opt.name ++ maybe "" (\a -> "=" ++ prettyPrintArgument a) opt.arg
  where name (Flag c)   = "-" ++ fromChar c
        name (Long n)   = "--" ++ n
        name (Full c n) = "-" ++ fromChar c ++ ", --" ++ n

prettyPrintArgument :: Argument -> String
prettyPrintArgument (Argument { name: n, default: d })
  = n ++ maybe "" (\v -> " [default: " ++ v ++  "]") d

argument :: String -> Maybe String -> Argument
argument name default = Argument { name: name, default: default }

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

    defaults :: L.TokenParser String
    defaults = L.default

    option :: L.TokenParser Desc
    option = do
      xopt@(Option opt) <- start

      description <- do
        flip P.manyTill (L.eof <|> (P.lookAhead $ P.try $ void start)) do
          P.choice $ P.try <$> [
            Default <$> defaults
          , L.anyToken *> pure Text
          ]

      let defaults = flip filter description \s -> case s of Default _ -> true
                                                             _         -> false
          default = maybe Nothing (\(Default v) -> Just v) $ head defaults

      if (length defaults > 1)
         then P.fail $ "Option " ++ (show $ prettyPrintOption xopt)
                    ++ " has multiple defaults!"
         else return unit

      if (isJust default) && (not $ isJust opt.arg)
         then P.fail $ "Option " ++ (show $ prettyPrintOption xopt)
                    ++ " does not take arguments. Cannot specify defaults."
         else return unit

      OptionDesc <$> do
        either P.fail return $ setDefault xopt default

      where

        setDefault :: Option -> Maybe String -> Either String Option
        setDefault (Option o) d = return $ Option $
          o { arg = do
                (Argument arg) <- o.arg
                return $ Argument $ arg { default = d } }

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
          return $ Option { name: Flag opt.flag
                          , arg:  do a <- opt.arg
                                     return $ argument a Nothing }

        long :: L.TokenParser Option
        long = do
          opt <- lopt
          return $ Option { name: Long opt.name
                          , arg:  do a <- opt.arg
                                     return $ argument a Nothing }

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
                return $ Option { name: Full f n, arg: arg }

              where
                combineArg (Just a) (Just b) | (a == b) = Right $ Just a
                combineArg Nothing  (Just b)            = Right $ Just b
                combineArg (Just a) Nothing             = Right $ Just a
                combineArg Nothing Nothing              = Right $ Nothing
                combineArg (Just a) (Just b) | (a /= b) = Left  $
                        "Arguments mismatch: " ++ (show $ prettyPrintArgument a)
                                    ++ " and " ++ (show $ prettyPrintArgument b)

    sopt :: L.TokenParser { flag :: Char, arg :: Maybe String }
    sopt = do
      opt <- L.sopt
      (guard $ (A.length opt.stack == 0)) P.<?> "No stacked options"
      return { flag: opt.flag, arg: opt.arg }

    lopt :: L.TokenParser { name :: String, arg :: Maybe String }
    lopt = L.lopt
