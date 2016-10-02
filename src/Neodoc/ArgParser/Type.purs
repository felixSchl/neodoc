module Neodoc.ArgParser.Type (
  ParseError (..)
, Parser (..)
, Step (..)
, Result
, IsConsumed
, unParser
, runParser
, getConfig
, getState
, setState
, modifyState
, getInput
, setInput
, fail
, fail'
, fatal
, fatal'
, throw
, catch
, catch'
, extractError
, mapError

-- `ArgParser`
, Input
, ArgParser
, ArgParseError(..)
, unexpectedInputError
, missingArgumentsError
, optionTakesNoArgumentError
, optionRequiresArgumentError
, malformedInputError
, genericError
, internalError
, IsKnown
, known
, unknown
, isKnown
, isUnknown
, unIsKnown
, ParseConfig
, ParseState
, findDescription
, lookupDescription
, lookupDescription'
, setFailed
, unsetFailed
, hasFailed
, hasTerminated
, setDone
, unsetDone
, skipIf
, setDepth
, modifyDepth
) where

import Prelude
import Data.List (List(..))
import Data.Either (Either(..), either)
import Data.Bifunctor (rmap)
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
import Data.List (head, filter, (:))
import Data.Pretty (pretty, class Pretty)
import Data.Foldable (any, intercalate)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty (singleton) as NonEmpty
import Control.Alt ((<|>))
import Data.Lazy (Lazy, defer, force)
import Neodoc.Env
import Neodoc.ArgParser.Token as Token
import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError, toNeodocError)
import Neodoc.Data.Description (Description(..))

data ParseError e = ParseError Boolean (Either String e)

mapError f (ParseError b e) = ParseError b (rmap  f e)

instance showParseError :: (Show e) => Show (ParseError e) where
  show (ParseError b e) = "ParseError " <> show b <> " "<> show e

instance prettyParseError :: (Pretty e) => Pretty (ParseError e) where
  pretty (ParseError false e) = either id pretty e
  pretty (ParseError true e) = "Fatal: " <> either id pretty e

type IsConsumed = Boolean
type Result e a = Either (ParseError e) a
data Step e c s i a = Step IsConsumed c s i (Result e a)
data Parser e c s i a = Parser (c -> s -> i -> Step e c s i a)

setConsumed :: ∀ e c s i a. Boolean -> Step e c s i a -> Step e c s i a
setConsumed b (Step _ c s i a) = Step b c s i a

setConsumedOr :: ∀ e c s i a. Boolean -> Step e c s i a -> Step e c s i a
setConsumedOr b (Step b' c s i a) = Step (b || b') c s i a

unParser :: ∀ e c s i a. Parser e c s i a -> c -> s -> i -> Step e c s i a
unParser (Parser f) = f

instance applyParser :: Apply (Parser e c s i) where
  apply = ap

instance applicativeParser :: Applicative (Parser e c s i) where
  pure a = Parser \c s i -> Step false c s i (Right a)

instance functorParser :: Functor (Parser e c s i) where
  map f p = Parser \c s i ->
    let step = unParser p c s i
     in case step of (Step b c' s' i' a) -> Step b c' s' i' (f <$> a)

instance bindParser :: Bind (Parser e c s i) where
  bind p f = Parser \c s i ->
    let step = unParser p c s i
     in case step of
          Step b c' s' i' (Left  err) -> Step b c' s' i' (Left err)
          Step b c' s' i' (Right res) -> setConsumedOr b $ unParser (f res) c' s' i'

instance monadParser :: Monad (Parser e c s i)

catch :: ∀ e c s i a. Parser e c s i a -> (s -> ParseError e -> Parser e c s i a) -> Parser e c s i a
catch p f = Parser \c s i ->
  let step = unParser p c s i
   in case step of
      Step consumed _ s' _ (Left (e@(ParseError fatal _)))
        | not (fatal || consumed) -> unParser (f s' e) c s i
      _ -> step

catch' :: ∀ e c s i a. (s -> ParseError e -> Parser e c s i a) -> Parser e c s i a -> Parser e c s i a
catch' = flip catch

throw :: ∀ e c s i a. ParseError e -> Parser e c s i a
throw e = Parser \c s i -> Step false c s i (Left e)

instance altParser :: Alt (Parser e c s i) where
  alt p1 p2 = catch p1 (\_ _ -> p2)

instance plusParser :: Plus (Parser e c s i) where
  empty = fail "No alternative"

instance alternativeParser :: Alternative (Parser e c s i)

instance lazyParser :: Lazy (Parser e c s i a) where
  defer f = Parser \c i -> unParser (f unit) c i

getConfig :: ∀ e c s i. Parser e c s i c
getConfig = Parser \c s i -> Step false c s i (Right c)

getState :: ∀ e c s i. Parser e c s i s
getState = Parser \c s i -> Step false c s i (Right s)

setState :: ∀ e c s i. s -> Parser e c s i Unit
setState s = Parser \c _ i -> Step false c s i (Right unit)

modifyState :: ∀ e c s i. (s -> s) -> Parser e c s i Unit
modifyState f = Parser \c s i -> Step false c (f s) i (Right unit)

getInput :: ∀ e c s i. Parser e c s i i
getInput = Parser \c s i -> Step false c s i (Right i)

setInput :: ∀ e c s i. i -> Parser e c s i Unit
setInput i = Parser \c s _ -> Step false c s i (Right unit)

runParser :: ∀ e c s i a. c -> s -> i -> Parser e c s i a -> Either (ParseError e) a
runParser c s i p =
  let step = unParser p c s i
   in case step of (Step _ _ _ _ r) -> r

fail :: ∀ e c s i a. String -> Parser e c s i a
fail message = Parser \c s i -> Step false c s i (Left $ ParseError false (Left message))

fail' :: ∀ e c s i a. e -> Parser e c s i a
fail' e = Parser \c s i -> Step false c s i (Left $ ParseError false (Right e))

fatal :: ∀ e c s i a. String -> Parser e c s i a
fatal message = Parser \c s i -> Step false c s i (Left $ ParseError true (Left message))

fatal' :: ∀ e c s i a. e -> Parser e c s i a
fatal' e = Parser \c s i -> Step false c s i (Left $ ParseError true (Right e))

extractError :: ∀ e . (String -> e) -> ParseError e -> e
extractError f (ParseError _ (Left  s)) = f s
extractError _ (ParseError _ (Right e)) = e

-- ArgParser:

type Input = List PositionedToken

data IsKnown a = Known a | Unknown a

instance showIsKnown :: (Show a) => Show (IsKnown a) where
  show (Known x) = "Known " <> show x
  show (Unknown x) = "Unknown " <> show x

instance prettyIsKnown :: (Pretty a) => Pretty (IsKnown a) where
  pretty (Known x) = pretty x
  pretty (Unknown x) = pretty x

known = Known
unknown = Unknown
isKnown (Known _) = true
isKnown (Unknown _) = false
isUnknown (Unknown _) = true
isUnknown (Known _) = false
unIsKnown (Unknown a) = a
unIsKnown (Known a) = a

data ArgParseError
  = OptionTakesNoArgumentError OptionAlias (Lazy String)
  | OptionRequiresArgumentError OptionAlias (Lazy String)
  | MissingArgumentsError (NonEmpty List SolvedLayout) (Lazy String)
  | UnexpectedInputError  (List SolvedLayout) (List (IsKnown PositionedToken)) (Lazy String)
  | MalformedInputError String (Lazy String)
  | GenericError String
  | InternalError String (Lazy String)

tokLabel :: PositionedToken -> String
tokLabel (PositionedToken { token, source }) = go token
  where
  go (Token.LOpt _ _)   = "option " <> source
  go (Token.SOpt _ _ _) = "option " <> source
  go (Token.EOA _)      = "option --"
  go Token.Stdin        = "option -"
  go (Token.Lit _)      = "command " <> source

optionTakesNoArgumentError a = OptionTakesNoArgumentError a $ defer \_ ->
  "option takes no argument: " <> pretty a

optionRequiresArgumentError a = OptionRequiresArgumentError a $ defer \_->
  "option requires argument: " <> pretty a

malformedInputError i = MalformedInputError i $ defer \_->
  "malformed input: " <> i

genericError msg = GenericError msg

internalError msg = InternalError msg $ defer \_->
  "internal error: " <> msg

unexpectedInputError expected toks
  = UnexpectedInputError expected toks $ defer \_ -> render expected toks
  where
  render Nil Nil = "" -- XXX: this shouldn't happen. can we encode this at type level?
  render Nil ((Known tok):_) = "unexpected " <> tokLabel tok
  render xs ((Unknown tok):_) = "unknown " <> tokLabel tok
  render (x:_) toks = "expected " <> pretty x <> butGot toks
  butGot Nil = ""
  butGot xs = ", but got " <> intercalate " " (pretty <$> xs)

missingArgumentsError layouts
  = MissingArgumentsError layouts $ defer \_ ->
      "missing " <> intercalate ", " (pretty <$> layouts)

instance showArgParseError :: Show ArgParseError where
  show (OptionTakesNoArgumentError a msg) = "OptionTakesNoArgumentError " <> show a <> " " <> show msg
  show (OptionRequiresArgumentError a msg) = "OptionRequiresArgumentError " <> show a <> " " <> show msg
  show (MissingArgumentsError xs msg) = "MissingArgumentsError " <> show xs <> " " <> show msg
  show (UnexpectedInputError xs ys msg) = "UnexpectedInputError " <> show xs <> " " <> show ys <> " " <> show msg
  show (MalformedInputError s msg) = "MalformedInputError " <> show s <> " " <> show msg
  show (GenericError s) = "GenericError " <> show s
  show (InternalError s msg) = "InternalError " <> show s <> " " <> show msg

instance prettyArgParseError :: Pretty ArgParseError where
  pretty (OptionTakesNoArgumentError _ msg) = force msg
  pretty (OptionRequiresArgumentError _ msg) = force msg
  pretty (MissingArgumentsError _ msg) = force msg
  pretty (UnexpectedInputError _ _ msg) = force msg
  pretty (MalformedInputError _ msg) = force msg
  pretty (GenericError s) = s
  pretty (InternalError _ msg) = force msg

-- XXX: this is a hacky instance for now
instance toNeodocErrorArgParseError :: ToNeodocError ArgParseError where
  toNeodocError x = Neodoc.ArgParserError (pretty x)

type ParseConfig r = {
  env :: Env
, options :: Options r
, descriptions :: List Description
}

type ParseState = {
  depth :: Int
, hasTerminated :: Boolean
, hasFailed :: Boolean
}

type ArgParser r a = Parser ArgParseError (ParseConfig r) ParseState Input a

-- XXX: This could be more efficient using a table lookup
findDescription :: OptionAlias -> List Description -> Maybe Description
findDescription alias descriptions = head $ filter matchesAlias descriptions
  where
  matchesAlias (OptionDescription aliases _ _ _ _) = any (_ == alias) aliases
  matchesAlias _ = false

lookupDescription :: ∀ r. OptionAlias -> ArgParser r (Maybe Description)
lookupDescription alias = do
  { descriptions } <- getConfig
  pure $ findDescription alias descriptions

lookupDescription' :: ∀ r. OptionAlias -> ArgParser r Description
lookupDescription' a = fromMaybe default <$> lookupDescription a
  where default = OptionDescription (NonEmpty.singleton a) false Nothing Nothing Nothing

unsetFailed :: ∀ r. ArgParser r Unit
unsetFailed = modifyState \s -> s { hasFailed = false }

setFailed :: ∀ r. ArgParser r Unit
setFailed = modifyState \s -> s { hasFailed = true }

hasFailed :: ∀ r. ArgParser r Boolean
hasFailed = _.hasFailed <$> getState

hasTerminated :: ∀ r. ArgParser r Boolean
hasTerminated = _.hasTerminated <$> getState

unsetDone :: ∀ r. ArgParser r Unit
unsetDone = modifyState \s -> s { hasTerminated = false }

setDone :: ∀ r. ArgParser r Unit
setDone = modifyState \s -> s { hasTerminated = true }

skipIf :: ∀ r a. ArgParser r Boolean -> a -> ArgParser r a -> ArgParser r a
skipIf a b c = a >>= if _ then pure b else c

modifyDepth :: ∀ r. (Int -> Int) -> ArgParser r Unit
modifyDepth f = modifyState \s -> s { depth = f s.depth }

setDepth :: ∀ r. Int -> ArgParser r Unit
setDepth = modifyDepth <<< const
