module Language.Docopt.RichValue (
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
import Data.Pretty (class Pretty, pretty)
import Language.Docopt.Value (Value(), prettyPrintValue)
import Language.Docopt.Origin (Origin())

-- | The value type the parser collects
type RichValueObj = {
  value  :: Value
, origin :: Origin
}

newtype RichValue = RichValue RichValueObj

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

instance showRichValue :: Show RichValue where
  show (RichValue v) = "(RichValue { origin: " <> show v.origin
                               <> ", value: "  <> show v.value
                               <> "})"

instance eqRichValue :: Eq RichValue where
  eq (RichValue v) (RichValue v') = v.origin == v'.origin
                                 && v.value  == v'.value

from :: Origin -> Value -> RichValue
from o v = RichValue $ { value: v, origin: o }

instance prettyRichValue :: Pretty RichValue where
  pretty (RichValue v) = pretty v.value <> " " <> "(" <> show v.origin <> ")"
