{-
Insert implicit flags into the specification.
This ensures that e.g. `prog -v` or `prog -h` are always valid applications of
the program.

To do so, we quite simply add an extra usage top-level branch to the
specification: `[-h] [-v]`.
-}

module Neodoc.Solve.ImplicitFlags where

import Prelude
import Data.NonEmpty
import Neodoc.Spec
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Data.Array as A
import Data.String as String
import Data.Either (Either)
import Data.List ((:), List(Nil), fromFoldable, catMaybes)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Neodoc.OptionAlias (OptionAlias)
import Neodoc.OptionAlias as OptionAlias

implicitFlags
  :: List OptionAlias
  -> Spec SolvedLayout
  -> Either SolveError (Spec SolvedLayout)
implicitFlags flags (Spec (spec@{ layouts: x:|xs })) = pure $
  let y = toOption <$> fromFoldable flags
    in case y of
      Nil  -> Spec spec { layouts = x :| xs }
      z:zs -> Spec spec { layouts = (pure $ z :| zs) :| x : xs }
  where
  toOption a = Group true true (((Elem $ Option a Nothing true):|Nil):|Nil)

