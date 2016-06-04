-- |
-- | Docopt FFI surface.
-- |
-- | Entrypoints to be called from JS-land.
-- | Data input and output is santized for either language and functions are
-- | curried/uncurried as needed.
-- |

module Docopt.FFI (run, parse) where

import Prelude
import Data.Function (Fn2, mkFn2)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.List (fromList)
import Control.Monad.Eff (Eff())
import Data.Either (Either(..), either)
import Data.StrMap (StrMap())
import Control.Bind ((=<<))
import Control.Alt (alt)
import Data.Foreign (readArray, typeOf, toForeign) as F
import Data.Foreign (Foreign, F, ForeignError(..), typeOf, unsafeFromForeign)
import Data.Foreign.Class (readProp) as F
import Language.Docopt.Usage (Usage(Usage))
import Language.Docopt.Argument (Branch(), Argument(..))
import Unsafe.Coerce (unsafeCoerce)

import Docopt as Docopt
import Language.Docopt.Value (Value(..))

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
run :: forall e.
      Fn2 String  -- ^ The docopt text
          Foreign -- ^ The options (optional)
          (Eff (Docopt.DocoptEff e) (StrMap RawValue))
run = mkFn2 go
  where
    go helpText fopts = do

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

            -- enable "smart-options" parsing. This causes singleton groups that
            -- "look like" they are describing an option to expand to such an
            -- option, e.g.: '[-o ARG]' becomes '[-o=ARG]'.
          , smartOptions = either (const Docopt.defaultOptions.smartOptions) id
                            (isTruthy <$> do
                              F.readProp "smartOptions" fopts)

            -- don't exit the process upon failure. By default, neodoc will
            -- exit the program if an error occured, right after printing the
            -- help text alongside an error message.
          , dontExit = either (const Docopt.defaultOptions.dontExit) id
                            (isTruthy <$> do
                              F.readProp "dontExit" fopts)
          }

      result <- Docopt.run helpText opts
      pure $ rawValue <$> result

      where
        toMaybe :: forall a b. Either a b -> Maybe b
        toMaybe e = either (const Nothing) (pure <<< id) e

        readObject :: Foreign -> F (StrMap Foreign)
        readObject value | isObject value = pure $ unsafeFromForeign value
        readObject value = Left (TypeMismatch "object" (typeOf value))

        isObject :: Foreign -> Boolean
        isObject f = F.typeOf f == "object"

type Node = { type :: String }

parse :: forall e.
        Fn2 String  -- ^ The docopt text
            Foreign -- ^ The options (optional)
            (Eff (Docopt.DocoptEff e) (Array (Array (Array Foreign))))
parse = mkFn2 go
  where
    go helpText opts = do
      let opts' = Docopt.defaultOptions {
            -- enable "smart-options" parsing. This causes singleton groups that
            -- "look like" they are describing an option to expand to such an
            -- option, e.g.: '[-o ARG]' becomes '[-o=ARG]'.
            smartOptions = either (const Docopt.defaultOptions.smartOptions) id
                            (isTruthy <$> do
                              F.readProp "smartOptions" opts)
          }
      result <- Docopt.parse helpText opts'
      pure $ fromList $ result <#> \(Usage branches) -> do
        fromList $ convBranch <$> branches

    convBranch :: Branch -> Array Foreign
    convBranch args = fromList $ convArg <$> args

    convArg :: Argument -> Foreign
    convArg (EOA)          = F.toForeign { type: "EOA" }
    convArg (Stdin)        = F.toForeign { type: "Stdin" }
    convArg (Command x)    = F.toForeign { type: "Command", value: x }
    convArg (Positional x) = F.toForeign { type: "Positional", value: x }
    convArg (Group x)      = F.toForeign {
      type: "Group"
    , value: {
        optional:   x.optional
      , repeatable: x.repeatable
      , branches:   (fromList $ convBranch <$> x.branches) :: Array (Array Foreign)
      }
    }
    convArg (Option x)     = F.toForeign {
      type: "Option"
    , value: {
        flag:       maybe undefined F.toForeign x.flag
      , name:       maybe undefined F.toForeign x.name
      , env:        maybe undefined F.toForeign x.env
      , repeatable: x.repeatable
      , arg:        maybe undefined (\a -> {
                      name:     a.name
                    , default:  maybe undefined F.toForeign a.default
                    , optional: a.optional
                    }) x.arg
      }
    }


foreign import undefined :: forall a. a
