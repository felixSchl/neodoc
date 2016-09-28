module Neodoc.Data.OptionArgument where

import Prelude
import Data.Pretty (class Pretty, pretty)
import Data.Maybe (Maybe, maybe)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.String (singleton) as String
import Data.NonEmpty (NonEmpty)
import Neodoc.Data.Layout

data OptionArgument
  = OptionArgument
      String  -- name
      Boolean -- optional

instance prettyOptionArgument :: Pretty OptionArgument where
  pretty (OptionArgument n o) = (if o then "[" else "")
                              <> n
                              <> (if o then "]" else "")

instance showOptionArgument :: Show OptionArgument where
  show (OptionArgument n o) = "OptionArgument " <> show n <> " " <> show o

instance eqOptionArgument :: Eq OptionArgument where
  eq (OptionArgument n o) (OptionArgument n' o') = n == n' && o == o'

isOptionArgumentOptional :: OptionArgument -> Boolean
isOptionArgumentOptional (OptionArgument _ o) = o
