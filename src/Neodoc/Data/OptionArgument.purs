module Neodoc.Data.OptionArgument where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe, maybe)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty)
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Neodoc.Data.Layout

data OptionArgument
  = OptionArgument
      String  -- name
      Boolean -- optional

derive instance eqOptionArgument :: Eq OptionArgument
derive instance ordOptionArgument :: Ord OptionArgument

instance isForeigntOptionArgument :: IsForeign OptionArgument where
  read v = OptionArgument
    <$> F.readProp "name" v
    <*> F.readProp "optional" v

instance asForeigntOptionArgument :: AsForeign OptionArgument where
  write (OptionArgument name optional) = F.toForeign { name, optional }

instance prettyOptionArgument :: Pretty OptionArgument where
  pretty (OptionArgument n o)
    = (if o then "[" else "") <> n <> (if o then "]" else "")

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument n o) = "OptionArgument " <> show n <> " " <> show o

isOptionArgumentOptional :: OptionArgument -> Boolean
isOptionArgumentOptional (OptionArgument _ o) = o
