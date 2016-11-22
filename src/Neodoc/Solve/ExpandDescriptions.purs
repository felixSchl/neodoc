module Neodoc.Solve.ExpandDescriptions where

import Prelude
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import Data.NonEmpty.Extra as NonEmpty
import Partial.Unsafe
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.Description
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Solve.Error

expandDescriptions
  :: ∀ a
   . Spec a
  -> Either SolveError (Spec a)
expandDescriptions (Spec (spec@{ descriptions })) = do
  Right (Spec $ spec { descriptions = expandDescription <$> descriptions })

  where
  -- TODO: do we only expand these if `mA` is Nothing or Optional?
  -- TODO: should this expansion be opt-in?
  expandDescription :: Description -> Description
  expandDescription (OptionDescription as r mA d e) =
    let as' = NonEmpty.concat $ as <#> \a ->
                OptionAlias.setNegative true a :|
                  OptionAlias.setNegative false a :
                    Nil
     in OptionDescription as' r mA d e
  expandDescription d = d
