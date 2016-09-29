-- | Transform a `ExpandedOptionsLayout` into a `SolvedLayout`
-- |
-- | This transform expands `[...-options]` reference tags into real options.
-- | This transform must be careful not to expand options that are already
-- | present in the same "free" grouping. We define a "free" grouping of such
-- | elements whose position does not matter.
-- |
-- | Since `[...-options]` reference tags strictly evaluate to "free" elements,
-- | that is short- and long options (practically speaking), the position of
-- | insertion does *NOT* matter, that is it must not necessarily occur in place
-- | of the actual tag, as long as it is within the same grouping.
-- |
-- | The method of performing this transform is roughly as follows, where
-- | `<* E...  *>` denotes an "free" chunk of elements `E` and
-- | `<! E...  !>` denotes an "fixed" chunk of elements `E`:
-- |
-- | Each top-level branch is solved independently. Given the following
-- | help text:
-- |
-- |    usage: prog foo -a [-b (-d -e)] [options] [-f] bar
-- |       or: prog foo bar --qux
-- |    options:
-- |      -d
-- |      -z
-- |
-- | We proceed as follows (for each top-level):
-- |
-- | 1. flatten the entire top-level and annotate each element w/ it's index:
-- |
-- |        prog foo -a [-b (-d -e)] [options] [-f] bar
-- |
-- |    becomes:
-- |
-- |        prog (1:foo) (2:-a) (3:-b) (4:-d) (5:-e) (6:[options]) (7:-f) (8:bar)
-- |
-- | 2. we chunk the output, such that "free" elements appear next to one
-- |    another:
-- |
-- |        prog <!(1:foo)!> <*(2:-a) (3:-b) (4:-d) (5:-e) (6:[options])
-- |             (7:-f)*> <!(8:bar)!>
-- |
-- | 3. we then expand any `[options]` elements into it's surrounding. We must
-- |    make sure not to expand an option into it's surrounding that is already
-- |    present. since the options section defined an `-d` (see above), but it
-- |    is already present in the surrounding area, we only expand `-z`
-- |
-- |        prog <!(1:foo)!> <*(2:-a) (3:-b) (4:-d) (5:-e) (6:[options: -z])
-- |             (7:-f)*> <!(8:bar)!>
-- |
-- | 4. finally, we apply expansion to the actual tree, using the indices as
-- |    lookups into the tree structure:
-- |
-- |        prog foo -a [-b (-d -e)] ([-z]) [-f] bar

module Neodoc.Solve.ExpandReferences where

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
import Data.Foldable (any)
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
import Neodoc.Data.Indexed
import Neodoc.Data.Indexed as Indexed
import Neodoc.ArgKey.Class
import Neodoc.Solve.Error
import Neodoc.Solve.ExpandOptions
import Neodoc.Evaluate.Annotate

import Partial.Unsafe (unsafePartial)

type IndexedLayout a = Layout (IndexedLayoutArg a)
type IndexedLayoutArg a = Indexed a

expandReferences
  :: Spec ExpandedOptionsLayout
  -> Either SolveError (Spec SolvedLayout)
expandReferences (Spec { program, layouts, descriptions }) =
  let -- expand the references per top-level branch, then remove any branches
      -- that yieleded `Nothing`.
      layouts' = (catMaybes <$> _)
          $ fromFoldable
          $ (expandReferencesInBranch <$> _) <$> layouts
      layouts'' = case layouts' of
                    Nil    -> Nil :| Nil -- create empty top-level branch
                    x : xs ->   x :| xs
   in pure (Spec { program, layouts: layouts'', descriptions })

  where
  expandReferencesInBranch
    :: NonEmpty List ExpandedOptionsLayout
    -> Maybe _
  expandReferencesInBranch branch =
    let -- 1. assign an index from left to right, to each layout element
        indexedBranch = indexBranch branch

        -- 2. flatten the branch, removing any recursion
        flatBranch = flattenBranch indexedBranch

        -- 3. chunk the flat branch into fixed/free args
        chunks = chunkBranch flatBranch

        -- 4. for each chunk, find out the args that a reference tag needs to
        --    resolve to. References are resolved left to right.
        --    (XXX: should they be aware of each other, so not to expand
        --          duplicates if they are adjacent? this would mean we'd need
        --          to run this in e.g. the State monad)
        indexToArgs = Indexed.toMap $ concat
                        $ fromFoldable $ expandChunk descriptions <$> chunks

        -- 6. expand the reference tags using the `index => [ arg ]` mapping.
        --    note that since branches could now be empty, we must be use a
        --    a slightly different intermediate structure that allows for empty
        --    lists.
        --    (XXX: it might be worth exploring if `Layout a` can be generalised
        --          to work on data structures other than `List a`s)
        expand = case _ of
          (EmptyableGroup o r xs) -> EmptyableGroup o r $ (expand <$> _) <$> xs
          (EmptyableElem (Indexed _ (SolvedArg x))) -> EmptyableElem x
          (EmptyableElem (Indexed ix (ReferenceArg n))) ->
            EmptyableGroup false false $ maybe Nil (_:Nil) do
              args <- Map.lookup ix indexToArgs
              pure $ args <#> \arg ->
                EmptyableGroup true false ((EmptyableElem arg:Nil):Nil)

     in toStrictBranch $ toEmptyableBranch indexedBranch <#> expand

expandChunk
  :: List Description
  -> Chunk _
  -> List (Indexed (List SolvedLayoutArg))
expandChunk _ (Fixed _) = Nil
expandChunk descriptions (Free xs) =
  let partitioned = partition xs
      surrounding = toArgKey <$> mlefts partitioned
      references = mrights partitioned
      isSurroundedBy k = any (_ == k) surrounding

   in references <#> \(Indexed ix n) ->
      let
        -- XXX: currently, references *always* refer to all descriptions
        --      this is work to be done, tracked in issue #57.
        descriptions' = descriptions

        -- 5. for each description associated with each reference,
        --    expand only those descriptions that are not already
        --    present in the same chunk. We determine presence based
        --    on all known `ArgKey`s of the argument.
        args = catMaybes $ descriptions' <#> case _ of
            (OptionDescription aliases@(a:|_) r mA _ _) ->
              if not $ any (isSurroundedBy <<< toArgKey) aliases
                then Just (Option a mA r)
                else Nothing
            _ -> Nothing
       in Indexed ix args

partition
  :: ∀ a b
   . List (Indexed ExpandedOptionsLayoutArg)
  -> List (Either SolvedLayoutArg (Indexed String))
partition xs = xs <#> case _ of
  Indexed _  (SolvedArg a)    -> Left a
  Indexed ix (ReferenceArg n) -> Right $ Indexed ix n

-- Chunk a branch
--   E(foo) G(-a -b -c) E(-x) => [Fixed([E(foo)]), Free([G(-a -b -c), E(-x)])]
chunkBranch
  :: NonEmpty     List (Indexed ExpandedOptionsLayoutArg)
  -> List (Chunk (List (Indexed ExpandedOptionsLayoutArg)))
chunkBranch = fromFoldable >>> chunk case _ of
  Indexed _ (ReferenceArg _) -> true
  Indexed _ (SolvedArg    a) -> Solved.isFreeLayout (Elem a)

-- Assign a numerical index to each element in a layout on the given branch,
-- as if the branch was completely flat, from left to right, i.e.:
--
--      -a (-b | -c)
--
-- would become:
--
--      (1:-a) ((2:-b) | (3:-c))
--
indexBranch
  :: ∀ a
   . NonEmpty List (Layout a)
  -> NonEmpty List (IndexedLayout a)
indexBranch branch = flip evalState 0 $ for branch indexLayout
  where
  indexLayout (Group o r xs) = Group o r <$> do
    for xs (traverse indexLayout)
  indexLayout (Elem x) = do
    i <- State.get
    State.put (i + 1)
    pure (Elem (Indexed i x))

-- Flatten a branch into a single list of elements, i.e.:
--
--      -a (-b | -c)
--
-- would become:
--
--      -a -b -c
--
flattenBranch
  :: ∀ a
   . NonEmpty List (Layout a)
  -> NonEmpty List a
flattenBranch branch = NonEmpty.concat $ flattenLayout <$> branch
  where
  flattenLayout (Elem x) = x :| Nil
  flattenLayout (Group _ _ branches)
    = NonEmpty.concat $ NonEmpty.concat $ (flattenLayout <$> _) <$> branches

