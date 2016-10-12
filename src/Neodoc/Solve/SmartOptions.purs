-- | Transform a `UsageLayout` into another `UsageLayout`
-- |
-- | This transform handles the smart-option mode of neodoc.
-- | 'smart-options' are those options that "look like" they should be binding
-- | the adjacent argument, but do not so explicitly.
-- |
-- | For example: "usage: prog [-f FILE]"
-- |
-- | Here, it is intuitively obvious that `-f` takes an option argument named
-- | `FILE`, but there is not mention of `-f` anywhere in a description section.
-- |
-- | This transform will bind `FILE` to `-f`, if and only if there is *NO*
-- | mention of `-f` anywhere in the descriptions, since if it was, that
-- | description should bind the argument explicitly.

module Neodoc.Solve.SmartOptions where

import Prelude

import Debug.Trace
import Data.List (List(..), (:), length, filter, null)
import Data.Array as Array
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (any)
import Data.Either (Either(..), either)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (for, traverse)

import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.Data.Layout
import Neodoc.Data.Layout as Layout
import Neodoc.Data.OptionArgument
import Neodoc.Data.UsageLayout
import Neodoc.Data.UsageLayout as Usage
import Neodoc.Data.Description
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Solve.Error
import Neodoc.Solve.ExpandOptions
import Neodoc.Solve.Traverse
import Data.NonEmpty.Extra (concat)

smartOptions
  :: Spec UsageLayout
  -> Either SolveError (Spec UsageLayout)
smartOptions (Spec (spec@{ layouts, descriptions })) = do
  layouts' <- for layouts (traverse (smartOptionsOnBranch descriptions))
  pure (Spec $ spec { layouts = layouts' })

  where
  smartOptionsOnBranch
    :: List Description
    -> Layout.Branch UsageLayoutArg
    -> Either SolveError (Layout.Branch UsageLayoutArg)
  smartOptionsOnBranch descriptions branch = pure $ go <$> branch
    where
    go g@(Group o r branches) = Group o r $ branches <#> case _ of
      xs@(x :| adjArg : Nil) -> fromMaybe xs do
        arg /\ argR <- case adjArg of
          Group o' r' ((x :| Nil) :| Nil) ->
            case x of
              (Elem (Usage.Command    n r)) -> Just $ OptionArgument n o' /\ (r || r')
              (Elem (Usage.Positional n r)) -> Just $ OptionArgument n o' /\ (r || r')
              _ -> Nothing
          (Elem (Usage.Command    n r)) -> Just $ OptionArgument n false /\ r
          (Elem (Usage.Positional n r)) -> Just $ OptionArgument n false /\ r
          _ -> Nothing

        opt <- case x of
          (Elem (Usage.Option n Nothing r)) ->
            let hasDescription = not (null $ findDescriptions (OptionAlias.Long n))
             in if hasDescription
                then Nothing
                else Just $ Option n (Just arg) (r || argR)

          (Elem (Usage.OptionStack (c :| cs) Nothing r)) ->

            -- transform: the last stacked char is the one to receive the explicit
            -- argument binding. the rest will be w/o any binding at all.
            -- ex: -abcdef=foo -> -a -b -c -d -e -f=foo
            let h = case (Array.last cs) /\ (Array.init cs) of
                      Just t /\ Just i -> t
                      _                -> c
                hasDescription = not (null $ findDescriptions (OptionAlias.Short h))
             in if hasDescription
                then Nothing
                else Just $ OptionStack (c :| cs) (Just arg) (r || argR)
          _ -> Nothing

        pure $ (Elem opt) :| Nil
      xs -> xs
    go x = x

    findDescriptions a = filter isMatch descriptions
      where
      isMatch (OptionDescription aliases _ _ _ _)
        = any (_ == a) aliases
      isMatch _ = false
