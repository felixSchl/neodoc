module Neodoc.ArgParser where

import Prelude
import Data.Either
import Data.Bifunctor (lmap)
import Data.List (fromFoldable)
import Neodoc.Env (Env)
import Neodoc.Spec (Spec)
import Neodoc.Data.SolvedLayout (SolvedLayout)
import Neodoc.ArgParser.Options (Options)
import Neodoc.ArgParser.Type (ParseError, ArgParser, ArgParseError(..), extractError)
import Neodoc.ArgParser.Parser (parse)
import Neodoc.ArgParser.Lexer as Lexer
import Text.Parsing.Parser (ParseError(..)) as P

run
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> Array String
  -> Either ArgParseError Unit
run spec opts env input = do
  toks <- runLexer $ Lexer.lex (fromFoldable input) opts
  runParser $ parse spec opts env toks

  where
  runLexer = lmap (MalformedInputError <<< getParseErrorMessage)
  runParser = lmap (extractError GenericError)

getParseErrorMessage :: P.ParseError -> String
getParseErrorMessage (P.ParseError s _ _) = s
