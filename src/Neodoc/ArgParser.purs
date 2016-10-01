module Neodoc.ArgParser (
  run
, module Reexports
) where

import Prelude
import Data.Either
import Data.Bifunctor (lmap)
import Data.List (fromFoldable)
import Neodoc.Env (Env)
import Neodoc.Spec (Spec)
import Neodoc.Data.SolvedLayout (SolvedLayout)
import Neodoc.ArgParser.Options (Options)
import Neodoc.ArgParser.Type (
  ParseError, ArgParser, ArgParseError, extractError, malformedInputError
, genericError)
import Neodoc.ArgParser.Parser (parse)
import Neodoc.ArgParser.Result (ArgParseResult(..))
import Neodoc.ArgParser.Lexer as Lexer
import Text.Parsing.Parser (ParseError(..)) as P

import Neodoc.ArgParser.Options as Reexports
import Neodoc.ArgParser.Result as Reexports

run
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> Array String
  -> Either ArgParseError ArgParseResult
run spec opts env input = do
  toks <- runLexer $ Lexer.lex (fromFoldable input) opts
  runParser $ parse spec opts env toks

  where
  runLexer = lmap (malformedInputError <<< getParseErrorMessage)
  runParser = lmap (extractError genericError)

getParseErrorMessage :: P.ParseError -> String
getParseErrorMessage (P.ParseError s _ _) = s
