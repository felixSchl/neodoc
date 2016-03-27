-- |
-- | Docopt FFI surface.
-- |
-- | Entrypoints to be called from JS-land.
-- | Data input and output is santized for either language and functions are
-- | curried/uncurried as needed.
-- |

module Docopt.FFI (
    run
  , defaultOptions
  ) where

import Prelude
import Debug.Trace
import Data.Function
import Docopt as Docopt

import Data.Either (either)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff.Console as Console
import Language.Docopt.Value (Value(..))
import Node.Process as Process
import Text.Wrap (dedent)

type RawValue = Unit

rawValue :: Value -> RawValue
rawValue (BoolValue   b) = unsafeCoerce b
rawValue (IntValue    i) = unsafeCoerce i
rawValue (FloatValue  x) = unsafeCoerce x
rawValue (StringValue s) = unsafeCoerce s
rawValue (ArrayValue xs) = unsafeCoerce $ rawValue <$> xs

run = mkFn2 _run
_run opts docopt = do
  result <- Docopt.run opts docopt
  either onError
          (return <<< (rawValue <$>))
          result
  where
    onError e = do
      Console.log $ dedent docopt
      Console.log e
      Process.exit 1

defaultOptions = Docopt.defaultOptions
