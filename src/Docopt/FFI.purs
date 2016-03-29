-- |
-- | Docopt FFI surface.
-- |
-- | Entrypoints to be called from JS-land.
-- | Data input and output is santized for either language and functions are
-- | curried/uncurried as needed.
-- |

module Docopt.FFI (run) where

import Prelude
import Debug.Trace
import Data.Function
import Docopt as Docopt
import Data.Foreign
import Data.Maybe (Maybe(..))
import Data.Foreign.Class (IsForeign)
import Control.Monad.Eff (Eff())
import Data.Either (Either(), either)
import Data.StrMap (StrMap())
import Control.Bind ((=<<))
import Control.Alt ((<|>))
import Data.Foreign       as F
import Data.Foreign.Index as F
import Data.Foreign.Class as F

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

_run :: forall e
      . String
     -> Foreign
     -> Eff (Docopt.DocoptEff e) (StrMap RawValue)
_run docopt opts = do

  let o = either (const Docopt.defaultOptions) id (readForeignOpts opts)
  result <- Docopt.run o docopt
  either onError (return <<< (rawValue <$>)) result

  where
    toMaybe e = either (const Nothing) (return <<< id) e

    readForeignOpts o =
      -- XXX: The `unsafeCoerce` should probably go..., but there is currently
      --      no `IsForeign` instance for `StrMap String`.
      let env  = (unsafeCoerce <$> (toMaybe $ F.prop "env" o)) :: Maybe (StrMap String)
          argv = (unsafeCoerce <$> (toMaybe $ F.readProp "argv" o)) :: Maybe (Array String)
       in return { argv: argv, env: env }

    onError e = do
      Console.log $ dedent docopt
      Console.log e
      Process.exit 1
