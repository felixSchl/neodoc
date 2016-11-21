module Neodoc.ArgParser.Type (
-- `ArgParser`
  Input
, ArgParser
, ArgParseError(..)
, Cache
, CachedStep

, MatchCache
, MatchCacheKey
, CachedMatch
-- , cachedMatch
-- , withLocalCaches

, ArgCache
, ArgCacheKey
, CachedArg
-- , cachedArg

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
, ArgParseState
, GlobalArgParseState
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
import Data.Optimize.Uncurried
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
import Neodoc.Value (Value(..))
import Neodoc.Parsing.Parser

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
  go (Token.LOpt _ _ _)   = "option " <> source
  go (Token.SOpt _ _ _ _) = "option " <> source
  go (Token.EOA _)        = "option --"
  go Token.Stdin          = "option -"
  go (Token.Lit _)        = "command " <> source

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

type ArgParseState = {
  depth :: Int -- how many elements have been consumed?
, hasTerminated :: Boolean -- have we terminated using `--` or `opts.stopAt`?
}

type GlobalArgParseState = {
  deepestError :: Maybe (Tuple Int ArgParseError)
, isKnownCache :: Map Token Boolean
, matchCache   :: MatchCache
, argCache     :: ArgCache
}

{- A general cache type -}
type Cache k v = Map k (CachedStep v)
data CachedStep v = CachedStep  Boolean
                                Unit -- do not restore config
                                ArgParseState
                                Unit -- do not restore global state
                                Input
                                (Result ArgParseError v)

{- A cache of matches -}
type MatchCache = Cache MatchCacheKey CachedMatch
type MatchCacheKey = Tuple (List Int) (Tuple Boolean (Tuple Boolean Input))
type CachedMatch = Tuple (List KeyValue)
                          (Tuple (List ArgParseLayout)
                                  (Tuple Boolean
                                          (Maybe ArgParseLayout)))

{- A cache of parsed args -}
type ArgCache = Cache ArgCacheKey CachedArg
type ArgCacheKey = Tuple Int Input
type CachedArg = Value

{- The arg parser type  -}
type ArgParser r a =
  Parser  ArgParseError
          (ParseConfig r)
          ArgParseState
          GlobalArgParseState
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

-- withLocalCaches :: ∀ r a. ArgParser r a -> ArgParser r a
-- withLocalCaches p = do
--   { matchCache, argCache } <- getGlobalState
--   modifyGlobalState \s -> s {
--     matchCache = Map.empty :: MatchCache
--   , argCache = Map.empty :: ArgCache
--   }
--   let reset = modifyGlobalState \s -> s {
--         matchCache = matchCache
--       , argCache = argCache
--       }
--   r <- p `catch` \_ e -> reset *> throw e
--   reset
--   pure r
--
-- cachedMatch
--   :: ∀ r
--    . List Int
--   -> Boolean
--   -> ArgParser r _
--   -> ArgParser r _
-- cachedMatch x b p = do
--   Parser \(args@(c /\ (s@{ hasTerminated }) /\ (g@{ matchCache }) /\ i)) ->
--     let key = x /\ b /\ hasTerminated /\ i
--      in case Map.lookup key matchCache of
--           Just (CachedStep b' _ _ _ i' result) ->
--             Step b' (c /\ s /\ g /\ i') result
--           Nothing ->
--             case unParser p args of
--               step@(Step b (c' /\ s' /\ g' /\ i') result) ->
--                 let step' = CachedStep b unit s' unit i' result
--                     cache' = Map.alter (const (Just step')) key g.matchCache
--                  in Step b (c' /\ s' /\ (g' { matchCache = cache' }) /\ i') result
--
-- cachedArg
--   :: ∀ r
--    . Int
--   -> ArgParser r CachedArg
--   -> ArgParser r CachedArg
-- cachedArg x p = do
--   Parser \(args@(c /\ s /\ (g@{ argCache }) /\ i)) ->
--     let key = x /\ i
--      in case Map.lookup key argCache of
--           Just (CachedStep b' _ _ _ i' result) ->
--             Step b' (c /\ s /\ g /\ i') result
--           Nothing ->
--             case unParser p args of
--               step@(Step b (c' /\ s' /\ g' /\ i') result) ->
--                 let step' = CachedStep b unit s' unit i' result
--                     cache' = Map.alter (const (Just step')) key g.argCache
--                  in Step b (c' /\ s' /\ (g' { argCache = cache' }) /\ i') result
