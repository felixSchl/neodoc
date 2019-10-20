module Neodoc.Data.EmptyableLayout where


import Prelude
import Data.Either (Either(..))
import Data.Array as Array
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, for)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra (concat)
import Data.List (List(..), (:), fromFoldable, length, catMaybes)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Foreign (F)
import Foreign as F
import Foreign.Index as F
import Foreign.Index ((!))
import Control.Monad.Except (throwError)

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

isEmpty :: ∀ a. EmptyableLayout a -> Boolean
isEmpty (EmptyableGroup _ _ Nil) = true
isEmpty _ = false

instance functorEmptyableLayout :: Functor EmptyableLayout where
  map f (EmptyableGroup o r xs) = EmptyableGroup o r $ ((f <$> _) <$> _) <$> xs
  map f (EmptyableElem x)       = EmptyableElem (f x)

instance showEmptyableLayout :: (Show a) => Show (EmptyableLayout a) where
  show (EmptyableElem  x)      = "EmptyableElem " <> show x
  show (EmptyableGroup o r xs) = "EmptyableGroup " <> show o <> " " <> show r <> " " <> show xs

-- instance isForeignEmptyableLayout :: (IsForeign a) => IsForeign (EmptyableLayout a) where
--   read v = do
--     typ :: String <- String.toUpper <$> F.readProp "type" v

--     case typ of
--       "ELEM" -> EmptyableElem
--         <$> F.readProp "elem" v
--       "GROUP" -> EmptyableGroup
--         <$> F.readProp "optional" v
--         <*> F.readProp "repeatable" v
--         <*> do
--             fromFoldable <$> do
--               (fromFoldable <$> _) <$> do
--                 F.readProp "branches" v :: F (Array (Array (EmptyableLayout a)))
--       _ -> F.fail $ F.errorAt "type" (F.JSONError $ "unknown type: " <> typ)


-- instance asForeignEmptyableLayout :: (AsForeign a) => AsForeign (EmptyableLayout a) where
--   write (EmptyableElem x) = F.toForeign {
--       type: "Elem"
--     , elem: F.write x
--     }
--   write (EmptyableGroup o r xs) = F.toForeign {
--       type: "Group"
--     , optional: F.write o
--     , repeatable: F.write r
--     , branches: Array.fromFoldable $ (Array.fromFoldable <<< (F.write <$> _)) <$> xs
--     }
