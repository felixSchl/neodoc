module Debug.Profile where

import Prelude
import Debug.Trace
import Data.Either
import Data.Tuple.Nested
import Data.Newtype (unwrap)
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Unsafe
import Text.Parsing.Parser (ParserT(..)) as P

_ENABLE_PROFILING_ :: Boolean
_ENABLE_PROFILING_ = false

prof :: ∀ m a. (Monad m) => String -> (Unit -> m a) -> m a
prof = profileA

profileA :: ∀ m a. (Monad m) => String -> (Unit -> m a) -> m a
profileA msg f =
  if _ENABLE_PROFILING_
    then do
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

foreign import timerStart :: ∀ eff. Eff eff Int
foreign import timerEnd :: ∀ eff. Int -> Eff eff Int
