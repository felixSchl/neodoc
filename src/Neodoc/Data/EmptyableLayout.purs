module Neodoc.Data.EmptyableLayout where


import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, for)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra (concat)
import Data.List (List(..), (:), fromFoldable, length, catMaybes)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Neodoc.Data.Layout


-- XXX: This type is required i.o to be able to have 0-length branches in case
--      an expansion yields an empty result. Ideally, we would use the same
--      data structure from `Data.Layout` (parameterised over the type of
--      container, where it's currently fixed to be `List a`)
type EmptyableBranch a = List (EmptyableLayout a)
data EmptyableLayout a
  = EmptyableGroup
      Boolean -- optional
      Boolean -- repeatable
      (List (EmptyableBranch a))
  | EmptyableElem a

instance functorEmptyableLayout :: Functor EmptyableLayout where
  map f (EmptyableGroup o r xs) = EmptyableGroup o r $ ((f <$> _) <$> _) <$> xs
  map f (EmptyableElem x)       = EmptyableElem (f x)

instance showEmptyableLayout :: (Show a) => Show (EmptyableLayout a) where
  show (EmptyableElem  x)      = "EmptyableElem " <> show x
  show (EmptyableGroup o r xs) = "Group " <> show o <> " " <> show r <> " " <> show xs

instance isForeignEmptyableLayout :: (IsForeign a) => IsForeign (EmptyableLayout a) where
  read v = do
    typ :: String <- F.readProp "type" v

    case typ of
      "Elem" -> EmptyableElem
        <$> F.readProp "elem" v
      "Group" -> EmptyableGroup
        <$> F.readProp "optional" v
        <*> F.readProp "repeatable" v
        <*> do
            fromFoldable <$> do
              (fromFoldable <$> _) <$> do
                F.readProp "branches" v :: F (Array (Array (EmptyableLayout a)))
      _ -> Left $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)

toStrictLayout
  :: ∀ a
   . EmptyableLayout a
  -> Maybe (Layout a)
toStrictLayout (EmptyableElem x) = Just (Elem x)
toStrictLayout (EmptyableGroup o r branches) =
  let branches' = catMaybes $ toStrictBranch <$> branches
   in case branches' of
        x : xs -> Just (Group o r (x :| xs))
        Nil    -> Nothing

toStrictBranch
  :: ∀ a
   . EmptyableBranch a
  -> Maybe (Branch a)
toStrictBranch branch =
  let elems = catMaybes $ toStrictLayout <$> branch
  in case elems of
        x : xs -> Just (x :| xs)
        Nil    -> Nothing

toEmptyableLayout
  :: ∀ a
   . Layout a
  -> EmptyableLayout a
toEmptyableLayout (Elem x) = EmptyableElem x
toEmptyableLayout (Group o r xs)
  = EmptyableGroup o r $ fromFoldable
                       $ toEmptyableBranch <$> xs

toEmptyableBranch
  :: ∀ a
   . Branch a
  -> EmptyableBranch a
toEmptyableBranch xs = fromFoldable $ toEmptyableLayout <$> xs
