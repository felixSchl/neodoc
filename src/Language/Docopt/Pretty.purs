module Language.Docopt.Pretty where

import Prelude
import Data.Maybe (maybe)
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar)
import Control.Apply ((*>))

import Language.Docopt.Types
import Language.Docopt.Value
import Language.Docopt.Argument
import qualified Language.Docopt.Option as O
