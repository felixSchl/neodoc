module Data.Foreign.Extra where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe
import Data.Either (either)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Map
import Data.NonEmpty (NonEmpty, (:|))
import Data.List (List(..), (:), fromFoldable)
import Data.List.NonEmpty as NEL
import Foreign
import Foreign.Index
import Control.Monad.Except (throwError, catchError)

prettyForeignError :: ForeignError -> String
prettyForeignError = go
  where
  go (ErrorAtIndex ix e) = "Error at [" <> show ix <> "]" <> nested e
  go (ErrorAtProperty prop e) = "Error at [" <> show prop <> "]" <> nested e
  go e = show e
  nested (ErrorAtIndex ix e) = "[" <> show ix <> "]" <> nested e
  nested (ErrorAtProperty prop e) = "[" <> show prop <> "]" <> nested e
  nested e = ": " <> show e

-- readPropMaybe :: forall a i. Index i => i -> Foreign -> F (Maybe a)
-- readPropMaybe k v = do
--   if hasOwnProperty k v
--   then do
--     x <- readProp k v
--     readNullOrUndefined x
--   else pure Nothing

-- defaultIfUndefined :: forall a i. Index i => i -> a -> Foreign -> F a
-- defaultIfUndefined k d v = do
--   v' :: Maybe Foreign <- readPropMaybe k v
--   maybe (pure d) unsafeToForeign v'

-- readList :: forall a. Foreign -> F (List a)
-- readList v = do
--   xs <- readArray v
--   pure $ fromFoldable xs

-- readNonemptyArray :: forall a. Foreign -> F (NonEmpty Array a)
-- readNonemptyArray v = do
--   xs <- readArray v
--   case Array.uncons xs of
--     Just { head, tail } -> pure $ head :| tail
--     _ -> fail $ ForeignError $ "array is empty"

-- readNonemptyList :: forall a. Foreign -> F (NonEmpty List a)
-- readNonemptyList v = do
--   xs <- readArray v
--   case fromFoldable xs of
--     head : tail -> pure $ head :| tail
--     _ -> fail $ ForeignError $ "list is empty"

-- | Is this Foreign value an object?
isObject :: Foreign -> Boolean
isObject f = typeOf f == "object"

-- | Interpret a foreign value as a JS dictionary
readObject :: Foreign -> F (Map String Foreign)
readObject value | isObject value = pure $ unsafeFromForeign value
readObject value = fail (TypeMismatch "object" (typeOf value))

foreign import undefined :: ∀ a. a
foreign import toString  :: ∀ a. a -> String
foreign import _isTruthy :: Foreign -> Boolean

newtype Truthy = Truthy Foreign

-- truthy :: ∀ a. a -> Truthy
-- truthy a = Truthy (toForeign a)

isTruthy :: Truthy -> Boolean
isTruthy (Truthy f) = _isTruthy f

-- instance isForeignTruthy :: IsForeign Truthy where
--   read v = pure $ Truthy v

errorAtIndex :: ∀ a. Int -> F a -> F a
errorAtIndex i f = catchError f \es -> fail $ ErrorAtIndex i (NEL.head es)
