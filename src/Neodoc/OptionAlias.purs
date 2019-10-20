module Neodoc.OptionAlias (
    OptionAlias (..)
  , Aliases ()
  , isLong
  , isShort
  , toAliasList
  , fromString
  , module NonEmpty
  ) where

import Prelude
import Data.Bifunctor (lmap, bimap)
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.String.CodeUnits (singleton, uncons) as String
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Pretty (class Pretty)
import Foreign as F
import Foreign.Index as F
import Foreign.Index ((!))

type Aliases = NonEmpty List OptionAlias
data OptionAlias = Short Char | Long String

derive instance eqOptionAlias :: Eq OptionAlias
derive instance ordOptionAlias :: Ord OptionAlias
derive instance genericOptionAlias :: Generic OptionAlias _

instance showOptionAlias :: Show OptionAlias where
  show = genericShow

-- instance isForeignOptionAlias :: IsForeign OptionAlias where
--   read v = do
--     s :: String <- read v
--     case fromString s of
--       Left  msg -> F.fail $ F.JSONError msg
--       Right s   -> pure s

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

-- instance asForeignOptionAlias :: AsForeign OptionAlias where
--   write (Short c) = F.toForeign $ "-"  <> (String.singleton c)
--   write (Long  n) = F.toForeign $ "--" <> n

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
