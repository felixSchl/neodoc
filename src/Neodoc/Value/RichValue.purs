module Neodoc.Value.RichValue (
  RichValue(..)
, RichValueObj()
, unRichValue
, from
, setValue
, setOrigin
, getValue
, getOrigin
) where

import Prelude
import Data.Generic.Rep
import Data.Pretty (class Pretty, pretty)
import Neodoc.Value (Value, prettyPrintValue)
import Neodoc.Value.Origin (Origin)

-- | The value type the parser collects
type RichValueObj = {
  value  :: Value
, origin :: Origin
}

newtype RichValue = RichValue {
  value  :: Value
, origin :: Origin
}

derive instance eqRichValue :: Eq RichValue
derive instance genericRichValue :: Generic RichValue

instance showRichValue :: Show RichValue where
  show = gShow

instance prettyRichValue :: Pretty RichValue where
  pretty (RichValue v) = pretty v.value <> " " <> "(" <> pretty v.origin <> ")"


unRichValue :: RichValue -> RichValueObj
unRichValue (RichValue o) = o

setValue :: RichValue -> Value -> RichValue
setValue (RichValue v) value = RichValue v { value = value }

setOrigin :: RichValue -> Origin -> RichValue
setOrigin (RichValue v) origin = RichValue v { origin = origin }

getValue :: RichValue -> Value
getValue (RichValue v) = v.value

getOrigin :: RichValue -> Origin
getOrigin (RichValue v) = v.origin

from :: Origin -> Value -> RichValue
from o v = RichValue $ { value: v, origin: o }
