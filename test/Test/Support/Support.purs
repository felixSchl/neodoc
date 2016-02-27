module Test.Support where

import Prelude
import Data.Map (Map())
import qualified Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Data.List (length)
import Data.Foldable (intercalate)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(), liftEff')
import Control.Monad.Eff
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)

runMaybeEff :: forall a eff. Maybe a -> Eff (err :: EXCEPTION | eff) a
runMaybeEff = maybe (throwException $ error "Nothing") pure

runEitherEff :: forall err a eff. (Show err) =>
  Either err a ->
  Eff (err :: EXCEPTION | eff) a
runEitherEff = either (throwException <<< error <<< show) pure

-- Run a effectful computation, but return unit
-- This is helpful to make a set of assertions in a Spec
vliftEff :: forall e. Eff e Unit -> Aff e Unit
vliftEff = void <<< liftEff

prettyPrintMap :: forall k v. Map k v
                            -> (k -> String)
                            -> (v -> String)
                            -> String
prettyPrintMap m pK pV =
  let xs = Map.toList m
    in if length xs == 0
          then "{}"
          else "{ "
            ++ (intercalate "\n, " $ xs <#> \(Tuple k v) ->
                                                pK k ++ " => " ++ pV v)
            ++ "\n}"
