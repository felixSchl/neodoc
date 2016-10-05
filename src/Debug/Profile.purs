module Debug.Profile where

import Prelude
import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Unsafe
import Text.Parsing.Parser (PState(..), ParserT(..), unParserT) as P


prof :: ∀ m a. (Monad m) => String -> (Unit -> m a) -> m a
prof = profileA

profileA :: ∀ m a. (Monad m) => String -> (Unit -> m a) -> m a
profileA msg f = do
  let t  = unsafePerformEff timerStart
  a <- f unit
  let t' = unsafePerformEff $ timerEnd t
  traceA $ msg <> " (" <> (show t') <> " ms)"
  pure a

profileParser :: ∀ s m a. (Monad m) => String -> P.ParserT s m a -> P.ParserT s m a
profileParser msg p = P.ParserT $ \s -> profileA msg \_ -> P.unParserT p s

foreign import timerStart :: ∀ eff. Eff eff Int
foreign import timerEnd :: ∀ eff. Int -> Eff eff Int
