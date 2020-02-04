module Neodoc.Data.OptionArgument where

import Prelude
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Pretty (class Pretty, pretty)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe, maybe)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty)
import Foreign (F)
import Foreign as F
import Foreign.Index as F
import Foreign.Index ((!))
import Neodoc.Data.Layout

data OptionArgument
  = OptionArgument
      String  -- name
      Boolean -- optional

derive instance eqOptionArgument :: Eq OptionArgument
derive instance ordOptionArgument :: Ord OptionArgument
derive instance genericOptionArgument :: Generic OptionArgument _

instance showOptionArgument :: Show OptionArgument where
  show = genericShow

-- instance isForeigntOptionArgument :: IsForeign OptionArgument where
--   read v = OptionArgument
--     <$> F.readProp "name" v
--     <*> F.readProp "optional" v

-- instance asForeigntOptionArgument :: AsForeign OptionArgument where
--   write (OptionArgument name optional) = F.toForeign { name, optional }

instance prettyOptionArgument :: Pretty OptionArgument where
  pretty (OptionArgument n o)
    = (if o then "[" else "") <> n <> (if o then "]" else "")

isOptionArgumentOptional :: OptionArgument -> Boolean
isOptionArgumentOptional (OptionArgument _ o) = o
