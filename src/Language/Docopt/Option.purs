module Language.Docopt.Option (
  Argument (..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Language.Docopt.Value (Value())
import qualified Data.String as Str

data Argument = Argument String (Maybe Value)

instance showArgument :: Show Argument where
  show (Argument n a) = (show n) ++ " " ++ (show a)

instance eqArgument :: Eq Argument where
  eq (Argument n a) (Argument n' a')
    = (Str.toUpper n == Str.toUpper n') && (a == a')

