module Data.Foreign.Extra where

import Prelude
import Data.Maybe
import Data.Either (Either(..))
import Data.Array as Array
import Data.NonEmpty (NonEmpty, (:|))
import Data.List (List(..), (:), fromFoldable)
import Data.Foreign
import Data.Foreign.Index
import Data.Foreign.Class

readPropMaybe :: forall a i. (IsForeign a, Index i) => i -> Foreign -> F (Maybe a)
readPropMaybe k v = do
  if hasOwnProperty k v
    then Just <$> readProp k v
    else pure Nothing

readList :: forall a. (IsForeign a) => Foreign -> F (List a)
readList v = do
  xs :: Array a <- read v
  pure $ fromFoldable xs

readNonemptyArray :: forall a. (IsForeign a) => Foreign -> F (NonEmpty Array a)
readNonemptyArray v = do
  xs :: Array a <- read v
  case Array.uncons xs of
    Just { head, tail } -> pure $ head :| tail
    _ -> Left $ JSONError $ "array is empty"

readNonemptyList :: forall a. (IsForeign a) => Foreign -> F (NonEmpty List a)
readNonemptyList v = do
  xs :: Array a <- read v
  case fromFoldable xs of
    head : tail -> pure $ head :| tail
    _ -> Left $ JSONError $ "list is empty"

foreign import undefined :: ∀ a. a
foreign import toString  :: ∀ a. a -> String
foreign import isTruthy :: Foreign -> Boolean
