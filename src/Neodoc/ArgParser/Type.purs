module Neodoc.ArgParser.Type (
-- `ArgParser`
  Input
, ArgParser
, ArgParseError(..)
, unexpectedInputError
, missingArgumentError
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
) where

import Prelude
import Debug.Trace
import Data.Optimize.Uncurried
import Data.Function.Memoize
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
import Neodoc.Value (Value(..))
import Neodoc.Parsing.Parser

-- ArgParser:

type Input = List PositionedToken

data IsKnown a = Known a | Unknown a

instance showIsKnown :: (Show a) => Show (IsKnown a) where
  show (Known x) = "(Known " <> show x <> ")"
  show (Unknown x) = "(Unknown " <> show x <> ")"

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
  | MissingArgumentError SolvedLayoutArg (Lazy String)
  | UnexpectedInputError (IsKnown PositionedToken) (Lazy String)
  | MalformedInputError String (Lazy String)
  | GenericError String
  | InternalError String (Lazy String)

tokLabel :: PositionedToken -> String
tokLabel = memoize go
  where
  go (PositionedToken token source _) = go token
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

unexpectedInputError tok
  = UnexpectedInputError tok $ defer \_ -> render tok
  where
  render (Known tok) = "unexpected " <> tokLabel tok
  render (Unknown tok) = "unknown " <> tokLabel tok

missingArgumentError arg
  = MissingArgumentError arg $ defer \_ ->
      "missing " <> pretty arg

instance showArgParseError :: Show ArgParseError where
  show (OptionTakesNoArgumentError a msg) = "(OptionTakesNoArgumentError " <> show a <> " " <> show msg <> ")"
  show (OptionRequiresArgumentError a msg) = "(OptionRequiresArgumentError " <> show a <> " " <> show msg <> ")"
  show (MissingArgumentError x msg) = "(MissingArgumentError " <> show x <> " " <> show msg <> ")"
  show (UnexpectedInputError x msg) = "(UnexpectedInputError " <> show x <> " " <> show msg <> ")"
  show (MalformedInputError s msg) = "(MalformedInputError " <> show s <> " " <> show msg <> ")"
  show (GenericError s) = "(GenericError " <> show s <> ")"
  show (InternalError s msg) = "(InternalError " <> show s <> " " <> show msg <> ")"

instance prettyArgParseError :: Pretty ArgParseError where
  pretty (OptionTakesNoArgumentError _ msg) = force msg
  pretty (OptionRequiresArgumentError _ msg) = force msg
  pretty (MissingArgumentError _ msg) = force msg
  pretty (UnexpectedInputError _ msg) = force msg
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

type ArgParseState = {}

type GlobalArgParseState = {}

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
