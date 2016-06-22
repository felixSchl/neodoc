module Debug.Profile where

import Prelude
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Unsafe

profileA :: forall m a. (Monad m) => String -> (Unit -> m a) -> m a
profileA msg f = do
  let t  = unsafePerformEff sampleTime
  a <- f unit
  let t' = unsafePerformEff sampleTime
  traceA $ msg <> " (" <> (show $ t' - t) <> "ms)"
  pure a

foreign import sampleTime :: forall eff. Eff eff Int
