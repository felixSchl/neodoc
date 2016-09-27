-- Reduce the neodoc output into a easily consume key => value map.
--
-- This is done by:
--      1. reducing the matched branch down to a set of arguments, merging them
--         as needed (lossy).
--      2. reducing the matched key-values down to a set of key -> [ value ]
--         (lossless)
--      3. applying values to the arguments in the branch processed in (1)
--         merging duplicate occurences as makes sense for that option
--      4. culling values considered empty

module Neodoc.Evaluate.Reduce (reduce) where

import Prelude
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.List (List(..), head, filter, singleton)
import Data.NonEmpty (NonEmpty)
import Data.Foldable (any, foldl, maximum, all)
import Neodoc.Env (Env)
import Neodoc.Data.Layout (Layout(..), Branch)
import Neodoc.Data.Description (Description(..))
import Neodoc.Data.SolvedLayout (SolvedLayout, SolvedLayoutArg)
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Value.RichValue
import Neodoc.ArgParser.KeyValue (KeyValue)
import Neodoc.ArgKey (ArgKey(..))
import Neodoc.ArgKey.Class (class ToArgKey, toArgKey)
import Neodoc.Evaluate.Annotate
import Neodoc.Evaluate.Key
import Partial.Unsafe (unsafePartial)

reduce
  :: Env
  -> List Description
  -> Branch SolvedLayoutArg
  -> List KeyValue
  -> Map ArgKey RichValue
reduce env descriptions branch vs =
  let annotedBranch = annotate descriptions <$> branch
   in Map.empty

setRepeatableOr
  :: Boolean
  -> SolvedLayout
  -> SolvedLayout
setRepeatableOr r l =
  -- optimal: use an if/else for short-circuiting
  if r  then Solved.setRepeatable r l
        else Solved.setRepeatable (r || Solved.isRepeatable l) l


-- fold f xs = foldl (Map.unionWith f) Map.empty xs

-- Expand a layout into a map of `Key => Argument`, where `Key` must uniquely
-- identify the argument in order to avoid loss.
-- expandLayout
--   :: Layout SolvedLayoutArg
--   -> List SolvedLayoutArg
expandLayout (Elem x) = Map.singleton (toKey x)
expandLayout (Group o r xs) =
  let y = 100 --fold inSameBranch (singleton Map.empty) -- <<< (expandLayout <$> _) <$> xs
   in Map.empty

  where
  inSameBranch :: SolvedLayoutArg -> SolvedLayoutArg -> SolvedLayoutArg
  inSameBranch x _ = x


--
-- -- Expand a layout into a map of `Key => Argument`, where `Key` must uniquely
-- -- identify the argument in order to avoid loss.
-- expandLayout
--   :: Layout SolvedLayoutArg
--   -> List SolvedLayoutArg
-- expandLayout (Elem x) = pure x
-- expandLayout (Group _ r xs) = Nil
--
--       reduceBranches grp.repeatable
--         $ ((flip D.setRepeatableOr grp.repeatable) <$> _)
--             <$> grp.branches


  -- where
  -- reduceBranches
  --   :: Boolean  -- ^ force repeatablity? This is used so that
  --               --   nested groups inherit super-group's
  --               --   repeatablity, collecting values into arrays.
  --   -> NonEmpty List (Branch SolvedLayoutArg)
  --   -> Unit
  -- reduceBranches r branches = unit
  --   let
  --     -- apply this group's repeatability to all elements across all branches
  --     -- inside
  --     xs' = (setRepeatableOr r <$> _) <$> xs
  --
  --   in Nil
  --
  --   

  -- flattenLayout <$> xs

-- Reduce a given `Branch a` 

-- -- Merge two arguments residing within the same branch
-- resolveInSameBranch :: SolvedLayoutArg -> SolvedLayoutArg -> SolvedLayoutArg
-- resolveInSameBranch (Solved.Option a _ _) (Solved.Option _ _ _) =
--   let 
--
--
-- -- resolveInSameBranch (D.Option o) (D.Option o')
-- --     = D.Option (o {
-- --                 arg = D.OptionArgument <$> do
-- --                   a  <- D.unOptionArgument <$> (o.arg  <|> o'.arg)
-- --                   a' <- D.unOptionArgument <$> (o'.arg <|> o.arg)
-- --                   pure {
-- --                     name:     a.name
-- --                   , default:  a.default  <|> a'.default
-- --                   , optional: a.optional || a'.optional
-- --                   }
-- --                 , repeatable = true
-- --                 })
-- -- resolveInSameBranch a b = D.setRepeatable a true
