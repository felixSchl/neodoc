module Neodoc.ArgParser.Type (
  ParseError (..)
, Parser (..)
, ParserArgs (..)
, Step (..)
, Result
, IsConsumed
, unParser
, runParser
, getConfig
, getState
, getGlobalState
, setState
, modifyState
, modifyGlobalState
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
, Cache
, CacheKey
, CachedStep
, cached
, withLocalCache
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
, GlobalParseState
, findDescription
, lookupDescription
, lookupDescription'
, hasTerminated
, setDone
, unsetDone
, skipIf
, setDepth
, modifyDepth
, setErrorAtDepth
) where

import Prelude
import Debug.Trace
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Token
import Data.Generic
import Neodoc.Env
import Neodoc.Data.LayoutConversion
import Data.Map as Map
import Data.Set as Set
import Neodoc.ArgParser.Lexer as L
import Neodoc.ArgParser.Token as Token
import Control.Alt ((<|>))
import Control.Lazy (class Lazy)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Foldable (any, intercalate)
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..))
import Data.List (head, filter, (:))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty (singleton) as NonEmpty
import Data.Pretty (pretty, class Pretty)
import Data.Set (Set)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

import Neodoc.Data.Description (Description(..))
import Neodoc.Data.Indexed (Indexed)
import Neodoc.Spec (Spec(..), Toplevel)
import Neodoc.Error (NeodocError(..)) as Neodoc
import Neodoc.Error.Class (class ToNeodocError, toNeodocError)
import Neodoc.OptionAlias (OptionAlias)
import Neodoc.ArgParser.KeyValue (KeyValue)
import Neodoc.ArgParser.Required (Required)
import Neodoc.ArgParser.ParseLayout

data ParseError e = ParseError Boolean (Either String e)

mapError f (ParseError b e) = ParseError b (rmap  f e)

instance showParseError :: (Show e) => Show (ParseError e) where
  show (ParseError b e) = "ParseError " <> show b <> " "<> show e

instance prettyParseError :: (Pretty e) => Pretty (ParseError e) where
  pretty (ParseError false e) = either id pretty e
  pretty (ParseError true e) = "Fatal: " <> either id pretty e

type ParserArgs c s g i = Tuple (Tuple (Tuple c s) g) i
type IsConsumed = Boolean
type Result e a = Either (ParseError e) a
data Step e c s g i a = Step IsConsumed c s g i (Result e a)
data Parser e c s g i a = Parser (ParserArgs c s g i -> Step e c s g i a)

setConsumed :: ∀ e c s g i a. Boolean -> Step e c s g i a -> Step e c s g i a
setConsumed b (Step _ c s g i a) = Step b c s g i a

setConsumedOr :: ∀ e c s g i a. Boolean -> Step e c s g i a -> Step e c s g i a
setConsumedOr b (Step b' c s g i a) = Step (b || b') c s g i a

unParser :: ∀ e c s g i a. Parser e c s g i a -> ParserArgs c s g i -> Step e c s g i a
unParser (Parser f) = f

instance applyParser :: Apply (Parser e c s g i) where
  apply = ap

instance applicativeParser :: Applicative (Parser e c s g i) where
  pure a = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Right a)

instance functorParser :: Functor (Parser e c s g i) where
  map f p = Parser \args ->
    let step = unParser p args
     in case step of (Step b c' s' g' i' a) -> Step b c' s' g' i' (f <$> a)

instance bindParser :: Bind (Parser e c s g i) where
  bind p f = Parser \args ->
    let step = unParser p args
     in case step of
          Step b c' s' g' i' (Left  err) -> Step b c' s' g' i' (Left err)
          Step b c' s' g' i' (Right res) -> setConsumedOr b $ unParser (f res) (c' /\ s' /\ g' /\ i')

instance monadParser :: Monad (Parser e c s g i)

catch :: ∀ e c s g i a. Parser e c s g i a -> (s -> ParseError e -> Parser e c s g i a) -> Parser e c s g i a
catch p f = Parser \(args@(c /\ s /\ g /\ i)) ->
  let step = unParser p args
   in case step of
      Step consumed _ s' g' _ (Left (e@(ParseError fatal _)))
        | not (fatal || consumed) -> unParser (f s' e) (c /\ s /\ g' /\ i)
      _ -> step

catch' :: ∀ e c s g i a. (s -> ParseError e -> Parser e c s g i a) -> Parser e c s g i a -> Parser e c s g i a
catch' = flip catch

throw :: ∀ e c s g i a. ParseError e -> Parser e c s g i a
throw e = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Left e)

instance altParser :: Alt (Parser e c s g i) where
  alt p1 p2 = catch p1 (\_ _ -> p2)

instance plusParser :: Plus (Parser e c s g i) where
  empty = fail "No alternative"

instance alternativeParser :: Alternative (Parser e c s g i)

instance lazyParser :: Lazy (Parser e c s g i a) where
  defer f = Parser \args -> unParser (f unit) args

getConfig :: ∀ e c s g i. Parser e c s g i c
getConfig = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Right c)

getState :: ∀ e c s g i. Parser e c s g i s
getState = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Right s)

setState :: ∀ e c s g i. s -> Parser e c s g i Unit
setState s = Parser \(c /\ _ /\ g /\ i) -> Step false c s g i (Right unit)

modifyState :: ∀ e c s g i. (s -> s) -> Parser e c s g i Unit
modifyState f = Parser \(c /\ s /\ g /\ i) -> Step false c (f s) g i (Right unit)

getGlobalState :: ∀ e c s g i. Parser e c s g i g
getGlobalState = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Right g)

modifyGlobalState :: ∀ e c s g i. (g -> g) -> Parser e c s g i Unit
modifyGlobalState f = Parser \(c /\ s /\ g /\ i) -> Step false c s (f g) i (Right unit)

getInput :: ∀ e c s g i. Parser e c s g i i
getInput = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Right i)

setInput :: ∀ e c s g i. i -> Parser e c s g i Unit
setInput i = Parser \(c /\ s /\ g /\ _) -> Step false c s g i (Right unit)

runParser :: ∀ e c s g i a. c -> s -> g -> i -> Parser e c s g i a -> Either (ParseError e) a
runParser c s g i p =
  let step = unParser p (c /\ s /\ g /\ i)
   in case step of (Step _ _ _ _ _ r) -> r

fail :: ∀ e c s g i a. String -> Parser e c s g i a
fail message = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Left $ ParseError false (Left message))

fail' :: ∀ e c s g i a. e -> Parser e c s g i a
fail' e = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Left $ ParseError false (Right e))

fatal :: ∀ e c s g i a. String -> Parser e c s g i a
fatal message = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Left $ ParseError true (Left message))

fatal' :: ∀ e c s g i a. e -> Parser e c s g i a
fatal' e = Parser \(c /\ s /\ g /\ i) -> Step false c s g i (Left $ ParseError true (Right e))

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
tokLabel (PositionedToken token source _) = go token
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
  butGot (x:_) = ", but got " <> pretty x

missingArgumentsError layouts
  = MissingArgumentsError layouts $ defer \_ ->
      let flat = flattenBranch layouts
       in case flat of
        layout:|_ -> "missing " <> pretty layout

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
, spec :: Spec SolvedLayout
}

type ParseState = {
  depth :: Int -- how many elements have been consumed?
, hasTerminated :: Boolean -- have we terminated using `--` or `opts.stopAt`?
}

type GlobalParseState = {
  deepestError :: Maybe (Tuple Int ArgParseError)
, isKnownCache :: Map Token Boolean
, cache :: Cache
}

type Cache = Map CacheKey CachedStep
type CacheKey = Tuple ((Tuple (Tuple (List Int) Boolean)) Boolean) Input
data CachedStep
  = CachedStep  Boolean
                Unit -- do not restore config
                ParseState
                Unit -- do not restore global state
                Input
                (Result ArgParseError (Tuple (Tuple (Tuple
                  (List KeyValue)
                  (List ArgParseLayout))
                  Boolean)
                  (Maybe ArgParseLayout)))

type ArgParser r a =
  Parser  ArgParseError
          (ParseConfig r)
          ParseState
          GlobalParseState
          Input
          a

-- XXX: This could be more efficient using a table lookup
findDescription :: OptionAlias -> List Description -> Maybe Description
findDescription alias descriptions = head $ filter matchesAlias descriptions
  where
  matchesAlias (OptionDescription aliases _ _ _ _) = any (_ == alias) aliases
  matchesAlias _ = false

lookupDescription :: ∀ r. OptionAlias -> ArgParser r (Maybe Description)
lookupDescription alias = do
  { spec } <- getConfig
  Spec { descriptions } <- pure spec
  pure $ findDescription alias descriptions

lookupDescription' :: ∀ r. OptionAlias -> ArgParser r Description
lookupDescription' a = fromMaybe default <$> lookupDescription a
  where default = OptionDescription (NonEmpty.singleton a) false Nothing Nothing Nothing

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

setErrorAtDepth :: ∀ r. Int -> ArgParseError -> ArgParser r Unit
setErrorAtDepth d e = do
  { deepestError } <- getGlobalState
  case deepestError of
    Just (d' /\ _) | d > d' -> modifyGlobalState \s -> s { deepestError = Just (d /\ e) }
    Nothing -> modifyGlobalState \s -> s { deepestError = Just (d /\ e) }
    _ -> pure unit

withLocalCache :: ∀ r a. ArgParser r a -> ArgParser r a
withLocalCache p = do
  { cache } <- getGlobalState
  modifyGlobalState \s -> s { cache = Map.empty :: Cache }
  let reset = modifyGlobalState \s -> s { cache = cache }
  r <- p `catch` \_ e -> reset *> throw e
  reset
  pure r

cached
  :: ∀ r
   . List Int
  -> Boolean
  -> ArgParser r _
  -> ArgParser r _
cached x b p = do
  Parser \(args@(c /\ (s@{ hasTerminated }) /\ (g@{ cache }) /\ i)) ->
    let key = x /\ b /\ hasTerminated /\ i
     in case Map.lookup key cache of
          Just (CachedStep b' _ _ _ i' result) ->
            Step b' c s g i' result
          Nothing ->
            case unParser p args of
              step@(Step b c' s' g' i' result) ->
                let step' = CachedStep b unit s' unit i' result
                    cache = Map.alter (const (Just step')) key g.cache
                 in Step b c' s' (g' { cache = cache }) i' result
