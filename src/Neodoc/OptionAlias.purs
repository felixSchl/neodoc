module Neodoc.OptionAlias (
    OptionAlias (..)
  , Aliases ()
  , isLong
  , isShort
  , toAliasList
  , fromString
  , IsNegative
  , isNegative
  , setNegative
  , module NonEmpty
  ) where

import Prelude
import Data.String as String
import Data.Bifunctor (lmap, bimap)
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
type IsNegative = Boolean
data OptionAlias = Short Char IsNegative | Long String IsNegative

derive instance eqOptionAlias :: Eq OptionAlias
derive instance ordOptionAlias :: Ord OptionAlias
derive instance genericOptionAlias :: Generic OptionAlias

instance showOptionAlias :: Show OptionAlias where
  show = gShow

instance isForeignOptionAlias :: IsForeign OptionAlias where
  read v = do
    s :: String <- read v
    case fromString s of
      Left  msg -> F.fail $ F.JSONError msg
      Right s   -> pure s

fromString :: String -> Either String OptionAlias
fromString s = case String.uncons s of
  Just { head: '+', tail } ->
    case String.uncons tail of
      Just { head, tail: "" } -> Right $ Short head true
      _ -> Left "short option must have a singe char"
  Just { head: '-', tail } ->
    case String.uncons tail of
      Just { head: '-', tail: "" } ->
        Left "long option must have a name"
      Just { head: '-', tail: tail' } -> Right
        let neg = String.take (String.length "no-") tail' == "no-"
         in Long tail' neg
      Just { head, tail: "" } -> Right $ Short head false
      _ -> Left "short option must have a singe char"
  Nothing -> Left "option may not be empty"
  _ -> Left "option must start with a dash"

instance asForeignOptionAlias :: AsForeign OptionAlias where
  write (Short c neg) = F.toForeign $ sign <> (String.singleton c)
    where sign = if neg then "+" else "-"
  write (Long  n neg) = F.toForeign $ sign <> n
    where sign = if neg then "--no-" else "--"

instance prettyOptionAlias :: Pretty OptionAlias where
  pretty (Short c neg) = sign <> (String.singleton c)
    where sign = if neg then "+" else "-"
  pretty (Long  n neg) = sign <> n
    where sign = if neg then "--no-" else "--"

isNegative :: OptionAlias -> Boolean
isNegative (Long _ neg) = neg
isNegative (Short _ neg) = neg

setNegative :: Boolean -> OptionAlias -> OptionAlias
setNegative neg (Long n _) = Long n neg
setNegative neg (Short c _) = Short c neg

isLong :: OptionAlias -> Boolean
isLong (Long _ _) = true
isLong _          = false

isShort :: OptionAlias -> Boolean
isShort (Short _ _) = true
isShort _           = false

toAliasList :: NonEmpty List OptionAlias -> List OptionAlias
toAliasList = fromNonEmpty (:)
