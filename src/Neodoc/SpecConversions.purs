module Neodoc.SpecConversions where

import Prelude
import Data.List (List(..), (:), null, fromFoldable, catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Foldable (intercalate)
import Data.Traversable (sequence)
import Data.Pretty (class Pretty, pretty)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Foreign (Foreign)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Data.Foreign (F)
import Control.Alt ((<|>))
import Neodoc.Spec
import Neodoc.Data.Layout
import Neodoc.Data.EmptyableLayout
import Neodoc.Data.LayoutConversion
import Neodoc.Data.Description

-- A specialized version to read a spec w/ potentialy empty branches which ought
-- to be pruned.
fromEmptyableSpec :: ∀ a. Spec (EmptyableLayout a) -> Spec (Layout a)
fromEmptyableSpec (Spec spec@{ layouts }) =
  let layouts' = layouts <#> \branches ->
        catMaybes $ fromFoldable $ branches <#> \branch ->
          let branch' = catMaybes $ fromFoldable $ branch <#> \layout -> toStrictLayout layout
            in case branch' of
              Nil  -> Nothing
              x:xs -> Just $ x:|xs
   in Spec (spec { layouts = layouts' })

toEmptyableSpec :: ∀ a. Spec (Layout a) -> Spec (EmptyableLayout a)
toEmptyableSpec (Spec spec) =
  let layouts = ((toEmptyableLayout <$> _) <$> _) <$> spec.layouts
   in Spec $ spec { layouts = layouts }
