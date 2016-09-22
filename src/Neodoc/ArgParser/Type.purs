module Neodoc.ArgParser.Type (
  ParseError (..)
, Parser (..)
, Step (..)
, Result
, unParser
, runParser
, getConfig
, getInput
, fail
, fail'
, Input
, ArgParser
, ArgParserError
, ParseConfig
) where

import Prelude
import Data.List (List)
import Data.Either (Either(..))
import Control.Lazy (class Lazy)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt)
import Neodoc.Spec (Spec)
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Neodoc.ArgParser.Lexer as L
import Neodoc.OptionAlias (OptionAlias)

-- `ArgParser`
import Neodoc.Env
import Neodoc.Data.Description (Description(..))

data ParseError e = ParseError Boolean (Either String e)

type Result e a = Either (ParseError e) a
data Step e c s a = Step c s (Result e a)
data Parser e c s a = Parser (c -> s -> Step e c s a)

unParser :: ∀ e c s a. Parser e c s a -> c -> s -> Step e c s a
unParser (Parser f) = f

instance applyParser :: Apply (Parser e c s) where
  apply = ap

instance applicativeParser :: Applicative (Parser e c s) where
  pure a = Parser \c s -> Step c s (Right a)

instance functorParser :: Functor (Parser e c s) where
  map f p = Parser \c s ->
    let step = unParser p c s
     in case step of (Step c s' a) -> Step c s' (f <$> a)

instance bindParser :: Bind (Parser e c s) where
  bind p f = Parser \c s ->
    let step = unParser p c s
     in case step of
          Step c s' (Left  err) -> Step c s' (Left err)
          Step c s' (Right res) -> unParser (f res) c s'

instance monadParser :: Monad (Parser e c s)

instance altParser :: Alt (Parser e c s) where
  alt p1 p2 = Parser \c s ->
    let step = unParser p1 c s
     in case step of
        Step _ _ (Left (ParseError fatal _)) | not fatal -> unParser p2 c s
        _ -> step

instance plusParser :: Plus (Parser e c s) where
  empty = fail "No alternative"

instance alternativeParser :: Alternative (Parser e c s)

instance lazyParser :: Lazy (Parser e c s a) where
  defer f = Parser \c s -> unParser (f unit) c s

getConfig :: ∀ e c s a. Parser e c s c
getConfig = Parser \c s -> Step c s (Right c)

getInput :: ∀ e c s a. Parser e c s s
getInput = Parser \c s -> Step c s (Right s)

runParser :: ∀ e c s a. c -> s -> Parser e c s a -> Either (ParseError e) a
runParser c s p =
  let step = unParser p c s
   in case step of (Step _ _ r) -> r

fail :: ∀ e c s a. String -> Parser e c s a
fail message = Parser \c s -> Step c s (Left $ ParseError false (Left message))

fail' :: ∀ e c s a. e -> Parser e c s a
fail' e = Parser \c s -> Step c s (Left $ ParseError false (Right e))

type Input = List PositionedToken

data ArgParserError
  = OptionTakesNoArgumentError OptionAlias
  | OptionRequiresArgumentError OptionAlias

type ParseConfig r = {
  env :: Env
, options :: Options r
, descriptions :: List Description
}

type ArgParser r a = Parser ArgParserError (ParseConfig r) Input a
