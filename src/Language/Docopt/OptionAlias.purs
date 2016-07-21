module Language.Docopt.OptionAlias (
    OptionAlias (..)
  , Aliases ()
  ) where

import Prelude
import Data.List (List(), (:))
import Data.Function (on)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Data.NonEmpty as NonEmpty

type Aliases = NonEmpty List OptionAlias
data OptionAlias = Short Char | Long String

instance showAlias :: Show OptionAlias where
  show (Short c) = "Short " <> show c
  show (Long  s) = "Long "  <> show s

instance eqAlias :: Eq OptionAlias where
  eq (Short c) (Short c') = c == c'
  eq (Long  s) (Long  s') = s == s'
  eq _         _          = false

instance ordAlias :: Ord OptionAlias where
  compare (Short c) (Short c') = c `compare` c'
  compare (Long  s) (Long  s') = s `compare` s'
  compare (Long  _) _          = GT -- move long names to the back
  compare (Short _) _          = LT -- move short names to the front

derive instance genericOptionAlias :: Generic OptionAlias

prettyPrintOptionAlias :: OptionAlias -> String
prettyPrintOptionAlias (Short c) = "-"  <> (String.singleton c)
prettyPrintOptionAlias (Long  n) = "--" <> n

isLong :: OptionAlias -> Boolean
isLong (Long _) = true
isLong _        = false

isShort :: OptionAlias -> Boolean
isShort (Short _) = true
isShort _         = false

toAliasList :: NonEmpty List OptionAlias -> List OptionAlias
toAliasList = fromNonEmpty (:)
