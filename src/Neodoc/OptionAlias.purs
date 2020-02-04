module Neodoc.OptionAlias (
    OptionAlias (..)
  , Aliases ()
  , isLong
  , isShort
  , toAliasList
  , fromString
  , module NonEmpty
  )
where

import Prelude

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty
  (NonEmpty(..), foldl1, fromNonEmpty, head, oneOf, singleton, tail, (:|))
  as NonEmpty
import Data.Pretty (class Pretty)
import Data.String.CodeUnits (singleton, uncons) as String


type Aliases = NonEmpty List OptionAlias

data OptionAlias = Short Char | Long String

derive instance eqOptionAlias :: Eq OptionAlias
derive instance ordOptionAlias :: Ord OptionAlias
derive instance genericOptionAlias :: Generic OptionAlias _

instance showOptionAlias :: Show OptionAlias where
  show = genericShow

instance encodeJsonOptionAlias :: EncodeJson OptionAlias where
  encodeJson option = encodeJson (toString option)

instance decodeJsonOptionAlias :: DecodeJson OptionAlias where
  decodeJson json = decodeJson json >>= fromString


toString :: OptionAlias -> String
toString (Short c) = "-"  <> (String.singleton c)
toString (Long  n) = "--" <> n


fromString :: String -> Either String OptionAlias
fromString s = case String.uncons s of
  Just { head: '-', tail } ->
    case String.uncons tail of
      Just { head: '-', tail: "" } ->
        Left "long option must have a name"
      Just { head: '-', tail: tail' } -> pure $ Long tail'
      Just { head, tail: "" } -> pure $ Short head
      _ -> Left "short option must have a singe char"
  Nothing -> Left "option may not be empty"
  _ -> Left "option must start with a dash"


instance prettyOptionAlias :: Pretty OptionAlias where
  pretty (Short c) = "-"  <> (String.singleton c)
  pretty (Long  n) = "--" <> n

isLong :: OptionAlias -> Boolean
isLong (Long _) = true
isLong _        = false

isShort :: OptionAlias -> Boolean
isShort (Short _) = true
isShort _         = false

toAliasList :: NonEmpty List OptionAlias -> List OptionAlias
toAliasList = fromNonEmpty (:)
