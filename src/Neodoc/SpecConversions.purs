module Neodoc.SpecConversions where

import Prelude (($), (<#>), (<$>))

import Data.List (catMaybes)
import Data.NonEmpty.Extra as NonEmpty

import Neodoc.Spec (Spec(..))
import Neodoc.Data.Layout (Layout)
import Neodoc.Data.EmptyableLayout (EmptyableLayout)
import Neodoc.Data.LayoutConversion (toEmptyableLayout, toStrictLayout)


-- A specialized version to read a spec w/ potentially empty branches
-- which ought to be pruned.
fromEmptyableSpec :: ∀ a. Spec (EmptyableLayout a) -> Spec (Layout a)
fromEmptyableSpec (Spec spec@{ layouts }) =
  let layouts' = layouts <#> \branches ->
        catMaybes $ branches <#> \branch ->
          NonEmpty.fromList $ catMaybes $
            NonEmpty.toList $ toStrictLayout <$> branch
   in Spec (spec { layouts = layouts' })


toEmptyableSpec :: ∀ a. Spec (Layout a) -> Spec (EmptyableLayout a)
toEmptyableSpec (Spec spec) =
  let layouts = ((toEmptyableLayout <$> _) <$> _) <$> spec.layouts
   in Spec $ spec { layouts = layouts }
