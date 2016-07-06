-- |
-- | Docopt FFI surface.
-- |
-- | Entrypoints to be called from JS-land.
-- | Data input and output is santized for either language and functions are
-- | curried/uncurried as needed.
-- |

module Docopt.FFI (
  run
, runFromSpec
, parse
, undefined
, readAsString
, readSpec
, specToForeign
) where

import Prelude
import Debug.Trace
import Data.Function.Uncurried
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.List (toUnfoldable, List(Nil), fromFoldable)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Tuple (Tuple())
import Data.Tuple.Nested ((/\))
import Data.Array (singleton) as Array
import Data.Traversable (traverse, for)
import Control.Monad.Eff (Eff())
import Data.Either (Either(..), either)
import Data.StrMap (StrMap())
import Control.Bind ((=<<))
import Control.Alt (alt, (<|>))
import Data.String (toChar) as String
import Data.Foreign (readBoolean, readChar, readArray, readString, typeOf, toForeign,
                    readInt, readNumber, unsafeReadTagged) as F
import Data.Foreign (Foreign, F, ForeignError(..), typeOf, unsafeFromForeign,
                    toForeign)
import Data.Foreign.Class (readProp) as F
import Data.Foreign.NullOrUndefined as F
import Language.Docopt.Argument (Branch(), Argument(..), OptionArgumentObj)
import Unsafe.Coerce (unsafeCoerce)

import Docopt as Docopt
import Language.Docopt (Specification(), Docopt())
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
-- | Run neodoc from JS.
-- |
run :: forall e.
  Fn2 String  -- ^ The neodoc text
      Foreign -- ^ The options (optional)
      (Eff (Docopt.DocoptEff e) (StrMap RawValue))
run = mkFn2 go
  where
    go helpText rOpts = do
      result <- Docopt.run (Right helpText) (readCommonOpts rOpts)
      pure $ rawValue <$> result

-- |
-- | Run neodoc from JS, provided a spec.
-- |
runFromSpec :: forall e.
  Fn2 Foreign -- ^ The neodoc spec
      Foreign -- ^ The options (optional)
      (Eff (Docopt.DocoptEff e) (StrMap RawValue))
runFromSpec = mkFn2 go
  where
    go rSpec rOpts = do
      case (readSpec rSpec) of
        Left e -> throwException (error (show e))
        Right spec -> do
          result <- Docopt.run (Left spec) (readCommonOpts rOpts)
          pure $ rawValue <$> result

-- | Interpret a foreign value as a JS dictionary
readObject :: Foreign -> F (StrMap Foreign)
readObject value | isObject value = pure $ unsafeFromForeign value
readObject value = Left (TypeMismatch "object" (typeOf value))

-- | Is this Foreign value an object?
isObject :: Foreign -> Boolean
isObject f = F.typeOf f == "object"

-- | Read common neodoc options
readCommonOpts :: Foreign -> Docopt.Options {}
readCommonOpts o = Docopt.defaultOptions {
    -- override argv with a custom array. Defaults to using `process.argv`
    argv = flip alt Docopt.defaultOptions.argv do
            toMaybe do
              F.readProp "argv" o

    -- override the environment with a custom hashmap. Defaults to using
    -- `process.env`
  , env = flip alt Docopt.defaultOptions.env do
            toMaybe do
              unsafeCoerce <$> do
                readObject =<< F.readProp "env" o

    -- enable "options-first" parsing. Options are only parsed and
    -- validated until the first operand (positional or command) is met.
    -- Trailing options are collected into a designated placeholder.
  , optionsFirst = either (const Docopt.defaultOptions.optionsFirst) id
                    (isTruthy <$> do
                      F.readProp "optionsFirst" o)

    -- enable "smart-options" parsing. This causes singleton groups that
    -- "look like" they are describing an option to expand to such an
    -- option, e.g.: '[-o ARG]' becomes '[-o=ARG]'.
  , smartOptions = either (const Docopt.defaultOptions.smartOptions) id
                    (isTruthy <$> do
                      F.readProp "smartOptions" o)

    -- stop parsing at these custom EOA markers. This allows any option
    -- to terminate a parse and collect all subsequent args.
  , stopAt = fromMaybe Docopt.defaultOptions.stopAt do
      toMaybe do
        p <- F.readProp "stopAt" o
        unsafeCoerce (F.readArray p) <|> do
          Array.singleton <$> F.readString p

    -- require flags to be explicitly passed? By default neodoc
    -- ignores missing flags during parsing argv.
  , requireFlags = either (const Docopt.defaultOptions.requireFlags) id
                    (isTruthy <$> do
                      F.readProp "requireFlags" o)

    -- don't exit the process upon failure. By default, neodoc will
    -- exit the program if an error occured, right after printing the
    -- help text alongside an error message.
  , dontExit = either (const Docopt.defaultOptions.dontExit) id
                        (isTruthy <$> do
                          F.readProp "dontExit" o)
  }

-- |
-- | Parse the help-text and return the spec as a JS value
-- |
parse :: forall e.
        Fn2 String  -- ^ The neodoc help-text
            Foreign -- ^ The options (optional)
            (Eff (Docopt.DocoptEff e) ({
              specification :: Array (Array (Array Foreign))
            , shortHelp     :: String
            , program       :: String
            }))
parse = mkFn2 go
  where
    go helpText fOpts = do
      let
        opts
          = Docopt.defaultOptions {
              -- enable "smart-options" parsing. This causes singleton groups
              -- that "look like" they are describing an option to expand to
              -- such an option, e.g.: '[-o ARG]' becomes '[-o=ARG]'.
              smartOptions
                = either (const Docopt.defaultOptions.smartOptions) id
                         (isTruthy <$> do
                           F.readProp "smartOptions" fOpts)
            }

      docopt <- Docopt.parse helpText opts
      pure $ specToForeign docopt

specToForeign
  :: Docopt
  -> { specification :: Array (Array (Array Foreign))
     , shortHelp     :: String
     , program       :: String
     }
specToForeign { shortHelp, specification, program } =
    let
      jsSpecification = toUnfoldable do
        specification <#> \branches -> do
          toUnfoldable do
            convBranch <$> branches

    in {
      shortHelp:     shortHelp
    , program:       program
    , specification: jsSpecification
    }

    where
    convBranch :: Branch -> Array Foreign
    convBranch args = toUnfoldable $ convArg <$> args

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
      , branches:   (toUnfoldable $ convBranch <$> x.branches) :: Array (Array Foreign)
      }
    }
    convArg (Option x) = F.toForeign {
      type: "Option"
    , value: {
        flag:       maybe undefined F.toForeign x.flag
      , name:       maybe undefined F.toForeign x.name
      , env:        maybe undefined F.toForeign x.env
      , repeatable: x.repeatable
      , arg:        maybe undefined (\a -> {
                      name:     a.name
                    , default:  maybe undefined rawValue a.default
                    , optional: a.optional
                    }) x.arg
      }
    }

-- |
-- | Parse a foreign value into a specification
-- |
readSpec :: Foreign -> F Docopt
readSpec input = do
  shortHelp  <- F.readProp "shortHelp" input
  program    <- F.readProp "program" input
  jsSpec     <- F.readProp "specification" input
  toplevel   <- F.readArray jsSpec
  spec       <- fromFoldable <$> do
    for toplevel \usage -> do
      branches <- F.readArray usage
      fromFoldable <$> do
        for branches \branch -> do
          args <- F.readArray branch
          fromFoldable <$> do
            for args readArg
  pure {
    program:       program
  , shortHelp:     shortHelp
  , specification: spec
  }

  where
  readNode :: Foreign -> F (Tuple String (Maybe Foreign))
  readNode o = (/\) <$> (F.readString =<< F.readProp "type" o)
                    <*> ((Just <$> F.readProp "value" o) <|> pure Nothing)

  readArg :: Foreign -> F (Argument)
  readArg o = do
    n <- readNode o
    case n of
      "Group" /\ (Just v) -> do
        grp <$> (isTruthy <$> F.readProp "optional" v)
            <*> (isTruthy <$> F.readProp "repeatable" v)
            <*> (fromFoldable <$> do
                  branches <- F.readArray =<< F.readProp "branches" v
                  for branches \branch -> do
                    args <- F.readArray branch
                    fromFoldable <$> do
                      for args readArg
                )
      "Command" /\ (Just v) -> do
        co <$> (readAsString =<< F.readProp "name" v)
           <*> (isTruthy <$> F.readProp "repeatable" v)
      "Positional" /\ (Just v) -> do
        po <$> (readAsString =<< F.readProp "name" v)
           <*> (isTruthy <$> F.readProp "repeatable" v)
      "Option" /\ (Just v) -> do
        opt <$> (ifHasProp v "flag" readFlag)
            <*> (ifHasProp v "name" readAsString)
            <*> (isTruthy <$> F.readProp "repeatable" v)
            <*> (ifHasProp v "env" readAsString)
            <*> (readOptArg v)
      "Stdin" /\ _ -> pure Stdin
      "EOA"   /\ _ -> pure EOA
      x /\ _ -> Left (TypeMismatch "Group, Command, Positional, Option, Stdin or EOA" x)

  readOptArg :: Foreign -> F (Maybe OptionArgumentObj)
  readOptArg v = "option-argument" <?> do
    mx <- nullOrUndefined (F.readProp "arg" v)
    case mx of
      Nothing -> pure Nothing
      Just x  -> Just <$> do
          arg <$> (ifHasProp' x "name" "<ARG>" readAsString)
              <*> (ifHasProp  x "default" readValue)
              <*> (isTruthy <$> F.readProp "optional" x)

  readFlag :: Foreign -> F Char
  readFlag v = "flag. Must be a single character." <?> do
      s <- readAsString v
      F.readChar (F.toForeign s)

  grp x y z     = Group      { optional: x, repeatable: y, branches: z }
  co  x y       = Command    { name: x, repeatable: y }
  po  x y       = Positional { name: x, repeatable: y }
  opt a b c d e = Option     { flag: a, name: b, repeatable: c, env: d, arg: e }
  arg x y z     = { name: x, default: y, optional: z }

readValue :: Foreign -> F Value
readValue x =
  (BoolValue   <$> F.readBoolean x) <|>
  (IntValue    <$> F.readInt     x) <|>
  (FloatValue  <$> F.readNumber  x) <|>
  (StringValue <$> F.readString  x) <|>
  (ArrayValue  <$> (F.readArray x >>= \vs -> for vs readValue))

optional :: forall a. F a -> F (Maybe a)
optional x = (Just <$> x) <|> pure Nothing

ifHasProp :: forall a. Foreign -> String -> (Foreign -> F a) -> F (Maybe a)
ifHasProp v s f = do
  mv <- nullOrUndefined (F.readProp s v)
  case mv of
    Nothing -> pure Nothing
    Just  v -> Just <$> f v

ifHasProp' :: forall a. Foreign -> String -> a -> (Foreign -> F a) -> F a
ifHasProp' v s o f = do
  mv <- ifHasProp v s f
  pure (fromMaybe o mv)

toMaybe :: forall a b. Either a b -> Maybe b
toMaybe e = either (const Nothing) (pure <<< id) e

readAsString :: Foreign -> F String
readAsString v = do
  (             (F.unsafeReadTagged "String"  v :: F String)) <|>
  (toString <$> (F.unsafeReadTagged "Boolean" v :: F String)) <|>
  (toString <$> (F.unsafeReadTagged "Number"  v :: F String))

nullOrUndefined :: F Foreign -> F (Maybe Foreign)
nullOrUndefined x = F.unNullOrUndefined <$> do
  F.readNullOrUndefined pure =<< x

infixl 9 expected as <?>
expected :: forall a. String -> F a -> F a
expected msg x = lmap (\_ -> JSONError $ "Invalid " <> msg) x

foreign import undefined :: forall a. a
foreign import toString  :: forall a. a -> String
