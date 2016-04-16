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
import Data.Tuple (Tuple(..))
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

-- | Convert a Value into a JS-native value.
rawValue :: Value -> RawValue
rawValue (BoolValue   b) = unsafeCoerce b
rawValue (IntValue    i) = unsafeCoerce i
rawValue (FloatValue  x) = unsafeCoerce x
rawValue (StringValue s) = unsafeCoerce s
rawValue (ArrayValue xs) = unsafeCoerce $ rawValue <$> xs

foreign import isTruthy :: Foreign -> Boolean

-- |
-- | Run docopt from JS.
-- |
run :: forall e
     .Fn2 String  -- ^ The docopt text
          Foreign -- ^ The options (optional)
          (Eff (Docopt.DocoptEff e) (StrMap RawValue))
run = mkFn2 go
  where
    go docopt fopts = do

      let opts = Docopt.defaultOptions {
            -- override argv with a custom array
            -- by default, this uses `process.argv`
            argv = flip alt Docopt.defaultOptions.argv do
                    toMaybe do
                      unsafeCoerce <$> do
                        F.readArray =<< F.readProp "argv" fopts

            -- override the environment with a custom hashmap.
            -- by default, this uses `process.env`
          , env = flip alt Docopt.defaultOptions.env do
                    toMaybe do
                      unsafeCoerce <$> do
                        readObject =<< F.readProp "env" fopts

            -- enable "options-first" parsing. Options are only parsed and
            -- validated until the first operand (positional or command) is
            -- met. Trailing options are collected into a designated
            -- placeholder.
          , optionsFirst = either (const Docopt.defaultOptions.optionsFirst) id
                            (isTruthy <$> do
                              F.readProp "optionsFirst" fopts)

            -- don't exit the process upon failure. By default, neodoc will
            -- exit the program if an error occured, right after printing the
            -- help text alongside an error message.
          , dontExit = either (const Docopt.defaultOptions.dontExit) id
                            (isTruthy <$> do
                              F.readProp "dontExit" fopts)
          }

      result <- Docopt.run docopt opts
      return $ rawValue <$> result

      where
        toMaybe :: forall a b. Either a b -> Maybe b
        toMaybe e = either (const Nothing) (return <<< id) e

        readObject :: Foreign -> F (StrMap Foreign)
        readObject value | isObject value = pure $ unsafeFromForeign value
        readObject value = Left (TypeMismatch "object" (typeOf value))

        isObject :: Foreign -> Boolean
        isObject f = F.typeOf f == "object"
