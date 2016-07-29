module Test.Support where

import Prelude
import Data.Map (Map())
import Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (length)
import Data.Foldable (intercalate)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(), liftEff')
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION(),
                                    catchException, Error())
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)

runMaybeEff :: ∀ a eff. Maybe a -> Eff (err :: EXCEPTION | eff) a
runMaybeEff = maybe (throwException $ error "Nothing") pure

runEitherEff :: ∀ err a eff. (Show err) =>
  Either err a ->
  Eff (err :: EXCEPTION | eff) a
runEitherEff = either (throwException <<< error <<< show) pure

-- Run a effectful computation, but return unit
-- This is helpful to make a set of assertions in a Spec
vliftEff :: ∀ e. Eff e Unit -> Aff e Unit
vliftEff = void <<< liftEff

prettyPrintMap :: ∀ k v. Map k v
                            -> (k -> String)
                            -> (v -> String)
                            -> String
prettyPrintMap m pK pV =
  let xs = Map.toList m
    in if length xs == 0
          then "{}"
          else "{ "
            <> (intercalate "\n, " $ xs <#> \(Tuple k v) -> pK k <> " => " <> pV v)
            <> "\n}"

type Assertion e = Eff (err :: EXCEPTION | e) Unit

assertEqual :: ∀ e a. (Eq a, Show a) => a -> a -> Assertion e
assertEqual expected actual =
  unless (actual == expected) (throwException (error msg))
    where msg = "expected: " <> show expected
             <> ", but got: " <> show actual

shouldEqual :: ∀ e a. (Eq a, Show a) => a -> a -> Assertion e
shouldEqual = flip assertEqual

assertThrows :: ∀ e a. (Error -> Boolean)
             -> Eff (err :: EXCEPTION | e) Unit -> Assertion e
assertThrows p m = do
  r <- unsafeInterleaveEff $ catchException (pure <<< pure) (Nothing <$ m)
  case r of
       Nothing -> throwException (error "Missing exception")
       Just  e ->
        if p e
          then pure unit
          else throwException (error $ "Expected an exception, but got: " <> show e)
