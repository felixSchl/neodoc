{-

# Simplify the spec by unnesting groups.

Consider:

* (a)       = a
* ((a))     = (a) = a
* ((a b c)) = (a b c)
* ([a])     = [a]
* [([a])]   = [a]
* [-abc]    = [-a] [-b] [-c]                                      (special case)
* [-abc -e] = [-a] [-b] [-c] [-e]                                 (special case)
* [-abc -e] = [-a] [-b] [-c] [-e]                                 (special case)
* [-abc  x] = [-abc x]                       (note: no expansion because of 'x')
* (a...)    = a...
* (a)...    = a...
* ((a)...)  = (a)... = a...
* ((a... b c)...) = ((a... b c)...)        (note: no expansion because of '...')
* ([a])     = [a]
* [([a])]   = [a]

This essentially canonicalises the specification. No meaning is lost in the
process. At least not in terms of parsing user input against the specification
later on.
-}

module Neodoc.Solve.Canonicalise where

import Prelude

import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (traverse, for)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NonEmpty
import Data.Map as Map
import Data.List (List(..), (:), fromFoldable, length, catMaybes, concat, filter)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Foldable (any, intercalate)
import Data.Pretty
import Control.Monad.State
import Control.Monad.State as State
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Control.Comonad (extract)

import Neodoc.Spec
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.EmptyableLayout
import Neodoc.Data.Description
import Neodoc.Data.Chunk
import Neodoc.Data.Layout
import Neodoc.Data.LayoutConversion
import Neodoc.Data.Indexed
import Neodoc.Data.Indexed as Indexed
import Neodoc.ArgKey.Class
import Neodoc.Solve.Error
import Neodoc.Solve.ExpandOptions
import Neodoc.Evaluate.Annotate

canonicalise
  :: Spec SolvedLayout
  -> Either SolveError (Spec SolvedLayout)
canonicalise (Spec (spec@{ layouts, descriptions })) =
  let layouts' = (canonicaliseBranch <$> _) <$> layouts
   in pure (Spec $ spec { layouts = layouts' })

  where
  canonicaliseBranch
    :: NonEmpty List SolvedLayout
    -> NonEmpty List SolvedLayout
  canonicaliseBranch branch = canonicaliseLayout <$> branch

  canonicaliseLayout
    :: SolvedLayout
    -> SolvedLayout

  canonicaliseLayout (Elem x) = Elem x

  -- (a) => a
  canonicaliseLayout (Group false r (((Elem x):|Nil):|Nil))
    | isPositional x ||
      isCommand    x ||
      (isOption    x &&
        not isFlag x &&  -- since flags are considered optional by default
        not r            -- mind repeating args (v0.7.0)
      )
    = setRepeatableOr r (Elem x)

  -- [[-a]] -> [-a]
  canonicaliseLayout (Group o r (((Group o' r' xs):|Nil):|Nil))
    = canonicaliseLayout (Group (o || o') (r || r') xs)

  -- [[-a] | [-b]] -> [-a | -b]
  canonicaliseLayout (Group o r branches) =
    let branches' = do
          branches <#> case _ of
            x :| Nil ->
              case canonicaliseLayout x of
                    Group o' r' (branch:|Nil) | o == o' && r == r' -> branch
                    a -> a :| Nil
            b -> canonicaliseLayout <$> b
     in Group o r branches'
