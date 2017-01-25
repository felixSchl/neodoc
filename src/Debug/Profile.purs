module Debug.Profile where

import Prelude
import Debug.Trace
import Data.Either
import Data.Tuple
import Data.Tuple.Nested
import Data.Newtype (unwrap)
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Unsafe

foreign import _ENABLE_PROFILING_ :: Boolean

prof :: ∀ m a. (Monad m) => String -> (Unit -> m a) -> m a
prof = profileA

measureA :: ∀ m a. (Monad m) => (Unit -> m a) -> m (Tuple Number a)
measureA f = do
  pure unit
  let t  = unsafePerformEff timerStart
  a <- f unit
  let t' = unsafePerformEff $ timerEnd t
  pure (t' /\ a)

profileA :: ∀ m a. (Monad m) => String -> (Unit -> m a) -> m a
profileA msg f =
  if _ENABLE_PROFILING_
    then do
      pure unit
      let t  = unsafePerformEff timerStart
      a <- f unit
      let t' = unsafePerformEff $ timerEnd t
      traceA $ msg <> " (" <> (show t') <> " ms)"
      pure a
    else f unit

profileS :: ∀ a. String -> (Unit -> a) -> a
profileS msg f =
  if _ENABLE_PROFILING_
    then
      let t' = unsafePerformEff timerStart
          c = \_->
                -- note: purescript appears to sort these assignments
                -- alphabetically, which means, we must name our variables
                -- accordingly. This depends on internal compiler behavior and
                -- may break w/ an update of purescript.
                let z = f unit
                    t = unsafePerformEff $ timerEnd t'
                 in z /\ t
       in case c unit of
            r /\ t -> trace (msg <> " (" <> (show t) <> " ms)") \_-> r
    else f unit

foreign import timerStart :: ∀ eff. Eff eff Number
foreign import timerEnd :: ∀ eff. Number -> Eff eff Number
