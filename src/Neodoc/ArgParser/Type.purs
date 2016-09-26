module Neodoc.ArgParser.Type (
  ParseError (..)
, Parser (..)
, Step (..)
, Result
, IsConsumed
, unParser
, runParser
, getConfig
, getInput
, setInput
, fail
, fail'
, throw
, catch
, catch'
, extractError

-- `ArgParser`
, Input
, ArgParser
, ArgParseError (..)
, ParseConfig
, lookupDescription
, lookupDescription'
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (head, filter)
import Data.Pretty (pretty, class Pretty)
import Data.Foldable (any, intercalate)
import Data.NonEmpty ((:|))
import Data.NonEmpty (singleton) as NonEmpty
import Control.Alt ((<|>))
import Neodoc.Env
import Neodoc.Data.Description (Description(..))

data ParseError e = ParseError Boolean (Either String e)

instance showParseError :: (Show e) => Show (ParseError e) where
  show (ParseError b e) = "ParseError " <> show b <> " "<> show e

type IsConsumed = Boolean
type Result e a = Either (ParseError e) a
data Step e c s a = Step IsConsumed c s (Result e a)
data Parser e c s a = Parser (c -> s -> Step e c s a)

setConsumed :: ∀ e c s a. Boolean -> Step e c s a -> Step e c s a
setConsumed b (Step _ c s a) = Step b c s a

setConsumedOr :: ∀ e c s a. Boolean -> Step e c s a -> Step e c s a
setConsumedOr b (Step b' c s a) = Step (b || b') c s a

unParser :: ∀ e c s a. Parser e c s a -> c -> s -> Step e c s a
unParser (Parser f) = f

instance applyParser :: Apply (Parser e c s) where
  apply = ap

instance applicativeParser :: Applicative (Parser e c s) where
  pure a = Parser \c s -> Step false c s (Right a)

instance functorParser :: Functor (Parser e c s) where
  map f p = Parser \c s ->
    let step = unParser p c s
     in case step of (Step b c' s' a) -> Step b c' s' (f <$> a)

instance bindParser :: Bind (Parser e c s) where
  bind p f = Parser \c s ->
    let step = unParser p c s
     in case step of
          Step b c' s' (Left  err) -> Step b c' s' (Left err)
          Step b c' s' (Right res) -> setConsumedOr b $ unParser (f res) c' s'

instance monadParser :: Monad (Parser e c s)

catch :: ∀ e c s a. Parser e c s a -> (ParseError e -> Parser e c s a) -> Parser e c s a
catch p f = Parser \c s ->
  let step = unParser p c s
   in case step of
      Step consumed _ _ (Left (e@(ParseError fatal _)))
        | not (fatal || consumed) -> unParser (f e) c s
      _ -> step

catch' :: ∀ e c s a. (ParseError e -> Parser e c s a) -> Parser e c s a -> Parser e c s a
catch' = flip catch

throw :: ∀ e c s a. ParseError e -> Parser e c s a
throw e = Parser \c s -> Step false c s (Left e)

instance altParser :: Alt (Parser e c s) where
  alt p1 p2 = catch p1 (const p2)

instance plusParser :: Plus (Parser e c s) where
  empty = fail "No alternative"

instance alternativeParser :: Alternative (Parser e c s)

instance lazyParser :: Lazy (Parser e c s a) where
  defer f = Parser \c s -> unParser (f unit) c s

getConfig :: ∀ e c s. Parser e c s c
getConfig = Parser \c s -> Step false c s (Right c)

getInput :: ∀ e c s. Parser e c s s
getInput = Parser \c s -> Step false c s (Right s)

setInput :: ∀ e c s. s -> Parser e c s Unit
setInput s = Parser \c _ -> Step false c s (Right unit)

runParser :: ∀ e c s a. c -> s -> Parser e c s a -> Either (ParseError e) a
runParser c s p =
  let step = unParser p c s
   in case step of (Step _ _ _ r) -> r

fail :: ∀ e c s a. String -> Parser e c s a
fail message = Parser \c s -> Step false c s (Left $ ParseError false (Left message))

fail' :: ∀ e c s a. e -> Parser e c s a
fail' e = Parser \c s -> Step false c s (Left $ ParseError false (Right e))

extractError :: ∀ e . (String -> e) -> ParseError e -> e
extractError f (ParseError _ (Left  s)) = f s
extractError _ (ParseError _ (Right e)) = e

type Input = List PositionedToken

data ArgParseError
  = OptionTakesNoArgumentError OptionAlias
  | OptionRequiresArgumentError OptionAlias
  | MissingArgumentsError (List SolvedLayout)
  | UnexpectedInputError (List PositionedToken)
  | MalformedInputError String
  | GenericError String
  | InternalError String

instance showArgParseError :: Show ArgParseError where
  show (OptionTakesNoArgumentError a) = "OptionTakesNoArgumentError " <> show a
  show (OptionRequiresArgumentError a) = "OptionRequiresArgumentError " <> show a
  show (MissingArgumentsError xs) = "MissingArgumentsError " <> show xs
  show (UnexpectedInputError xs) = "UnexpectedInputError " <> show xs
  show (MalformedInputError s) = "MalformedInputError " <> show s
  show (GenericError s) = "GenericError " <> show s
  show (InternalError s) = "InternalError " <> show s

instance prettyArgParseError :: Show ArgParseError where
  show (OptionTakesNoArgumentError a) = "Option takes no argument: " <> pretty a
  show (OptionRequiresArgumentError a) = "Option requires argument: " <> pretty a
  show (MissingArgumentsError xs) = "Missing arguments: " <> (intercalate ", " $ pretty <$> xs)
  show (UnexpectedInputError xs) = "Unexpected Input: " <> (intercalate " " $ show <$> xs)
  show (MalformedInputError s) = "Malformed Input: " <> show s
  show (GenericError s) = s
  show (InternalError s) = "Internal error: " <> s

type ParseConfig r = {
  env :: Env
, options :: Options r
, descriptions :: List Description
}

type ArgParser r a = Parser ArgParseError (ParseConfig r) Input a

-- XXX: This could be more efficient using a table lookup
lookupDescription :: ∀ r. OptionAlias -> ArgParser r (Maybe Description)
lookupDescription alias = do
  { descriptions } <- getConfig
  pure $ head $ filter matchesAlias descriptions

  where
  matchesAlias (OptionDescription aliases _ _ _ _) = any (_ == alias) aliases
  matchesAlias _ = false

lookupDescription' :: ∀ r. OptionAlias -> ArgParser r Description
lookupDescription' a = fromMaybe default <$> lookupDescription a
  where default = OptionDescription (NonEmpty.singleton a) false Nothing Nothing Nothing
