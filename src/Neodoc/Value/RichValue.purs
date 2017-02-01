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
import Data.Generic
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

setValue :: Value -> RichValue -> RichValue
setValue value (RichValue v) = RichValue v { value = value }

setOrigin :: Origin -> RichValue -> RichValue
setOrigin origin (RichValue v) = RichValue v { origin = origin }

getValue :: RichValue -> Value
getValue (RichValue v) = v.value

getOrigin :: RichValue -> Origin
getOrigin (RichValue v) = v.origin

from :: Origin -> Value -> RichValue
from o v = RichValue $ { value: v, origin: o }
