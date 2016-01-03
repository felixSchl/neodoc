module Docopt.Spec.Parser.Desc where

import Prelude
import Debug.Trace
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Data.List (
  List(..), some, (:), toList, length
, singleton, many, head, catMaybes)
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

import Docopt.Spec.Parser.Base
import Docopt.Spec.Parser.Common
import qualified Docopt.Spec.Parser.Lexer as L

data Desc = OptionDesc { flag    :: Maybe Char
                       , long    :: Maybe String
                       , arg     :: Maybe String
                       , default :: Maybe String }
          | CommandDesc

type Option_ = { flag :: Maybe Char
               , long :: Maybe String
               , arg  :: Maybe String }

derive instance genericDesc :: Generic Desc

instance showDesc :: Show Desc
  where show = gShow

instance eqDesc :: Eq Desc
  where eq = gEq

prettyPrintDesc :: Desc -> String
prettyPrintDesc (OptionDesc opt) = "" ++ rest
  where rest = (if (Str.length name > 0) then name else "<no-name>")
                  ++ arg
                  ++ default
        short     = maybe "" (\c -> "-" ++ Str.fromChar c) opt.flag
        long      = maybe "" ((maybe "--" (const ", --") opt.flag) ++) opt.long
        name      = short ++ long
        arg       = maybe "" ("=" ++) opt.arg
        extra n x = maybe "" (\v -> "\n       [" ++ n ++  ": " ++ v ++  "]") x
        default   = extra "default" opt.default

parse :: (List L.PositionedToken) -> Either P.ParseError (List Desc)
parse = flip L.runTokenParser descParser

descParser :: L.TokenParser (List Desc)
descParser = many option
  where
    anyName :: L.TokenParser String
    anyName = L.angleName <|> L.shoutName <|> L.name

    defaults :: L.TokenParser String
    defaults = L.default

    option :: L.TokenParser Desc
    option = do
      -- sameIndent
      opt <- P.choice $ P.try <$> [ both, long, short ]

      -- Parse one token at a time towards the next option or the eof.
      -- If a `[default: ...]` token is met, list it.
      default <- head <<< catMaybes <$> do
        flip P.manyTill (L.eof <|> (P.try $ P.lookAhead $ void option)) do
          P.choice $ P.try <$> [
            Just <$> defaults
          , L.anyToken *> pure Nothing
          ]

      return $ OptionDesc { flag:    opt.flag
                          , long:    opt.long
                          , arg:     opt.arg
                          , default: default }

      where
        short :: L.TokenParser Option_
        short = do
          opt <- sopt
          return $ { flag: pure opt.flag
                   , long: Nothing
                   , arg:  opt.arg }

        long :: L.TokenParser Option_
        long = do
          opt <- lopt
          return $ { flag: Nothing
                   , long: pure opt.name
                   , arg:  opt.arg }

        both :: L.TokenParser Option_
        both = markLine do
          sopt' <- sopt
          sameLine
          lopt' <- P.choice $ P.try <$> [ L.comma *> lopt , lopt ]
          arg <- resolve sopt'.arg lopt'.arg
          return $ { flag: pure sopt'.flag
                   , long: pure lopt'.name
                   , arg:  arg }

          where resolve (Just a) (Just b) | (a == b) = return $ Just a
                resolve Nothing  (Just b)            = return $ Just b
                resolve (Just a) Nothing             = return $ Just a
                resolve Nothing Nothing              = return $ Nothing
                resolve (Just a) (Just b) | (a /= b) = P.fail $
                  "Arguments mismatch: " ++ show a ++ " and " ++ show b

    sopt :: L.TokenParser { flag :: Char, arg :: Maybe String }
    sopt = do
      opt <- L.sopt
      (guard $ (A.length opt.stack == 0)) P.<?> "No stacked options"
      return { flag: opt.flag, arg: opt.arg }

    lopt :: L.TokenParser { name :: String, arg :: Maybe String }
    lopt = L.lopt
