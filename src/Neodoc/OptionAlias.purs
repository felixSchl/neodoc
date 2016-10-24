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
import Data.String as String
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Pretty (class Pretty)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F

type Aliases = NonEmpty List OptionAlias
data OptionAlias = Short Char | Long String

derive instance eqOptionAlias :: Eq OptionAlias
derive instance ordOptionAlias :: Ord OptionAlias
derive instance genericOptionAlias :: Generic OptionAlias

instance showOptionAlias :: Show OptionAlias where
  show = gShow

instance isForeignOptionAlias :: IsForeign OptionAlias where
  read v = do
    s :: String <- read v
    lmap F.JSONError (fromString s)

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

instance asForeignOptionAlias :: AsForeign OptionAlias where
  write (Short c) = F.toForeign $ "-"  <> (String.singleton c)
  write (Long  n) = F.toForeign $ "--" <> n

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
