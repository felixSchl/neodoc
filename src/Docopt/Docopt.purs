-- |
-- | Docopt utiltiy surface.
-- |
-- | Extract docopt sources from a README, parse and run it.
-- |

module Docopt where

import Prelude
import Control.Monad.Aff (Aff(), launchAff)
import Node.Path (FilePath())
import Control.Monad.Eff (Eff())
import Node.FS.Aff (readFile)

fromREADME :: forall eff
            . FilePath
           -> Eff eff Unit
fromREADME f = launchAff do
  pure unit


fromREADME' :: String -> Unit
fromREADME' f = unit
