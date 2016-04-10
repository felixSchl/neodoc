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
import Data.Maybe (Maybe(..), maybe)
import Data.Foreign.Class (IsForeign)
import Data.Either (Either(..))
import Data.Bifunctor (rmap)
import Control.Monad.Eff (Eff())
import Data.Either (Either(), either)
import Data.StrMap (StrMap())
import Control.Bind ((=<<))
import Control.Alt (alt, (<|>))
import Data.Foreign       as F
import Data.Foreign.Index as F
import Data.Foreign.Class as F
import Data.Foreign (Foreign(), F())

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

foreign import isTruthy :: Foreign -> Boolean

run = mkFn2 _run
_run :: forall e
      . String
     -> Foreign
     -> Eff (Docopt.DocoptEff e) (StrMap RawValue)
_run docopt opts = do
  let o = Docopt.defaultOptions {
            argv = flip alt Docopt.defaultOptions.argv do
                      toMaybe do
                        unsafeCoerce <$> do
                          F.readArray =<< F.readProp "argv" opts
          , env = flip alt Docopt.defaultOptions.env do
                      toMaybe do
                        unsafeCoerce <$> do
                          readObject =<< F.readProp "env" opts
          , optionsFirst = either (const Docopt.defaultOptions.optionsFirst)
                                  id
                                  do
                                    isTruthy <$> do
                                      F.readProp "optionsFirst" opts
          }
  result <- Docopt.run o docopt
  either onError (return <<< (rawValue <$>)) result

  where
    toMaybe :: forall a b. Either a b -> Maybe b
    toMaybe e = either (const Nothing) (return <<< id) e

    readObject :: Foreign -> F (StrMap Foreign)
    readObject value | isObject value = pure $ unsafeFromForeign value
    readObject value = Left (TypeMismatch "object" (typeOf value))

    isObject :: Foreign -> Boolean
    isObject f = F.typeOf f == "object"

    onError e = do
      Console.log e
      Process.exit 1
