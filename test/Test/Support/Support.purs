module Test.Support where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(), liftEff')
import Control.Monad.Eff
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

runMaybeEff :: forall a eff. Maybe a -> Eff (err :: EXCEPTION | eff) a
runMaybeEff m =
  case m of
    Just v  -> pure v
    Nothing -> throwException (error "Nothing")

runEitherEff :: forall err a eff. (Show err, Show a) =>
  Either err a ->
  Eff (err :: EXCEPTION | eff) a
runEitherEff m =
  case m of
    Right v  -> pure v
    Left err -> throwException (error $ show err)

-- Run a effectful computation, but return unit
-- This is helpful to make a set of assertions in a Spec
-- vliftEff = void <$> liftEff'
-- vliftEff x = void $ liftEff x
vliftEff :: forall e. Eff e Unit -> Aff e Unit
vliftEff x = void $ liftEff x
