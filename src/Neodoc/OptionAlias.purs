module Neodoc.OptionAlias (
    OptionAlias (..)
  , Aliases ()
  , isLong
  , isShort
  , toAliasList
  , module NonEmpty
  ) where

import Prelude
import Data.String as String
import Data.List (List(), (:))
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
    typ :: String <- String.toUpper <$> F.readProp "type" v

    case typ of
      "SHORT" -> Short <$> F.readProp "char" v
      "LONG"  -> Long  <$> F.readProp "name" v
      _ -> Left $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

instance asForeignOptionAlias :: AsForeign OptionAlias where
  write (Short c) = F.toForeign { type: "Short", char: c }
  write (Long  n) = F.toForeign { type: "Long", name: n }

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
