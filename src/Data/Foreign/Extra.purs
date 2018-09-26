module Data.Foreign.Extra where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe
import Data.Either (either)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Either (Either(..))
import Data.Array as Array
import Data.NonEmpty (NonEmpty, (:|))
import Data.List (List(..), (:), fromFoldable)
import Data.List.NonEmpty as NEL
import Data.Foreign
import Data.Foreign.NullOrUndefined
import Data.Foreign.Index
import Data.Foreign.Class
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

readPropMaybe :: forall a i. IsForeign a => Index i => i -> Foreign -> F (Maybe a)
readPropMaybe k v = do
  if hasOwnProperty k v
    then do
      x :: NullOrUndefined a <- readProp k v
      pure $ unNullOrUndefined x
    else pure Nothing

defaultIfUndefined :: forall a i. IsForeign a => Index i => i -> a -> Foreign -> F a
defaultIfUndefined k d v = do
  v' :: Maybe Foreign <- readPropMaybe k v
  maybe (pure d) read v'

readList :: forall a. (IsForeign a) => Foreign -> F (List a)
readList v = do
  xs :: Array a <- read v
  pure $ fromFoldable xs

readNonemptyArray :: forall a. (IsForeign a) => Foreign -> F (NonEmpty Array a)
readNonemptyArray v = do
  xs :: Array a <- read v
  case Array.uncons xs of
    Just { head, tail } -> pure $ head :| tail
    _ -> fail $ JSONError $ "array is empty"

readNonemptyList :: forall a. (IsForeign a) => Foreign -> F (NonEmpty List a)
readNonemptyList v = do
  xs :: Array a <- read v
  case fromFoldable xs of
    head : tail -> pure $ head :| tail
    _ -> fail $ JSONError $ "list is empty"

-- | Is this Foreign value an object?
isObject :: Foreign -> Boolean
isObject f = typeOf f == "object"

-- | Interpret a foreign value as a JS dictionary
readObject :: Foreign -> F (StrMap Foreign)
readObject value | isObject value = pure $ unsafeFromForeign value
readObject value = fail (TypeMismatch "object" (typeOf value))

foreign import undefined :: ∀ a. a
foreign import toString  :: ∀ a. a -> String
foreign import _isTruthy :: Foreign -> Boolean

newtype Truthy = Truthy Foreign

truthy :: ∀ a. a -> Truthy
truthy a = Truthy (toForeign a)

isTruthy :: Truthy -> Boolean
isTruthy (Truthy f) = _isTruthy f

instance isForeignTruthy :: IsForeign Truthy where
  read v = pure $ Truthy v

errorAtIndex :: ∀ a. Int -> F a -> F a
errorAtIndex i f = catchError f \es -> fail $ ErrorAtIndex i (NEL.head es)
