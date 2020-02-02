module Test.Support where

import Prelude
import Data.Map (Map(), toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.List (length)
import Data.Foldable (intercalate)
import Effect (Effect())
import Effect.Class (liftEffect)
import Effect.Aff (Aff())
import Effect.Exception (error, throwException, catchException, Error())
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)


runMaybeEff :: ∀ a. Maybe a -> Effect a
runMaybeEff =
  maybe (throwException $ error "Nothing") pure


runEitherEff :: ∀ err a. (Show err) =>
  Either err a ->
  Effect a
runEitherEff = either (throwException <<< error <<< show) pure


-- Run a effectful computation, but return unit
-- This is helpful to make a set of assertions in a Spec
vliftEff :: Effect Unit -> Aff Unit
vliftEff = void <<< liftEffect


prettyPrintMap
  :: ∀ k v. Map k v
  -> (k -> String)
  -> (v -> String)
  -> String
prettyPrintMap m pK pV =
  let xs = toUnfoldable m
  in if length xs == 0
    then "{}"
    else "{ "
      <> (intercalate "\n, " $ xs <#> \(Tuple k v) -> pK k <> " => " <> pV v)
      <> "\n}"


type Assertion e = Effect Unit


assertEqual :: ∀ e a. Eq a => Show a => a -> a -> Assertion e
assertEqual expected actual =
  unless (actual == expected) (throwException (error msg))
    where msg = "expected: " <> show expected
             <> ", but got: " <> show actual


shouldEqual :: ∀ e a. Eq a => Show a => a -> a -> Assertion e
shouldEqual = flip assertEqual


assertThrows
  :: ∀ e. (Error -> Boolean)
  -> Effect Unit -> Assertion e
assertThrows p m = do
  r <- catchException (pure <<< pure) (Nothing <$ m)
  case r of
    Nothing -> throwException (error "Missing exception")
    Just e ->
      if p e
      then pure unit
      else throwException (error $ "Expected an exception, but got: " <> show e)
