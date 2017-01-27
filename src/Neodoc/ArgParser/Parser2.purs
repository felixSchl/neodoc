module Neodoc.ArgParser.Parser2 where

import Prelude
import Data.List (fromFoldable)
import Data.NonEmpty.Extra as NEL
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.ArgParser.Pattern

layoutToPattern
  :: SolvedLayout
  -> Pattern Unit

layoutToPattern (Elem x) = case x of
  Command    n r -> LeafPattern false r     true  unit
  Positional n r -> LeafPattern false r     true  unit
  Option a mA r  -> LeafPattern false r     false unit
  EOA            -> LeafPattern false false false unit
  Stdin          -> LeafPattern false false false unit

-- TODO: detect `isFixed`. This can be done in a separate pass, lazily (stop
--       recursing as soon as first fixed arg is found.
layoutToPattern (Group o r xs) = ChoicePattern o r false do
  NEL.toList do
    ((layoutToPattern <$> _) <<< NEL.toList) <$> do
      xs
