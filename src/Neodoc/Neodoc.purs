module Neodoc (
  run
, run'
, runJS
, runPure
, runPure'
, parseHelpText
, parseHelpTextJS
, readSpec
, lookup
, lookup'
, Output (..)
, module Reexports
) where

import Prelude
import Debug.Profile
import Data.Array as A
import Data.Generic
import Data.Newtype (unwrap)
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Function.Uncurried
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Either (Either (..), either, fromRight)
import Data.StrMap (StrMap)
import Data.String as String
import Data.Char as Char
import Data.StrMap as StrMap
import Data.List (
  List(..), (:), many, toUnfoldable, concat, fromFoldable, catMaybes, filter
, length)
import Data.List.NonEmpty as NEL
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (for)
import Data.Pretty (class Pretty, pretty)
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Data.Foreign (F, Foreign)
import Data.Foreign.Class as F
import Data.Foreign.Extra as F
import Data.Foreign as F
import Data.Foldable (any, intercalate)
import Data.String.Yarn (lines, unlines)
import Control.Alt ((<|>))
import Control.Monad.Eff.Exception (Error, throwException, error, EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe
import Control.Monad.Except (throwError, catchError, runExcept)
import Node.Process (PROCESS)
import Node.Process as Process
import Node.FS (FS)
import Text.Wrap (dedent)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

import Neodoc.Spec
import Neodoc.Options
import Neodoc.Data.Layout
import Neodoc.Data.UsageLayout
import Neodoc.Data.LayoutConversion
import Neodoc.Data.EmptyableLayout
import Neodoc.Data.EmptyableLayout as Layout
import Neodoc.Error.Class (capture) as Error
import Neodoc.Error as Error
import Neodoc.Error (NeodocError)
import Neodoc.Scanner as Scanner
import Neodoc.SpecConversions as Spec
import Neodoc.Spec.Parser as Spec
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Solve as Solver
import Neodoc.Value (Value(..))
import Neodoc.ArgParser as ArgParser
import Neodoc.ArgParser (ArgParseResult(..))
import Neodoc.Evaluate as Evaluate
import Neodoc.Data.SolvedLayout
import Neodoc.Value as Value
import Neodoc.OptionAlias as OptionAlias

import Neodoc.Options as Reexports

_DEVELOPER_ERROR_MESSAGE :: String
_DEVELOPER_ERROR_MESSAGE = dedent """
  This is an error with the program itself and not your fault.
  Please bring this to the program author's attention.
"""

type NeodocEff e = (
    process :: PROCESS
  , err     :: EXCEPTION
  , console :: CONSOLE
  , fs      :: FS
  | e
)

data Output
  = VersionOutput (StrMap Value) String
  | Output        (StrMap Value)
  | HelpOutput    (StrMap Value) String

getArgs :: Output -> StrMap Value
getArgs (VersionOutput x _) = x
getArgs (Output          x) = x
getArgs (HelpOutput    x _) = x

lookup :: String -> Output -> Maybe Value
lookup k o = StrMap.lookup k (getArgs o)

lookup' :: String -> Output -> Either String Value
lookup' k o = case StrMap.lookup k (getArgs o) of
                  Just v -> Right v
                  _ -> case OptionAlias.fromString k of
                              Right _ -> Right (BoolValue false)
                              _       -> Left $ "no such key: " <> show k

instance prettyOutput :: Pretty Output where
  pretty (VersionOutput _ s) = s
  pretty (HelpOutput    _ s) = s
  pretty (Output          s) = pretty s

instance showOutput :: Show Output where
  show (VersionOutput x s) = "VersionOutput " <> show x <> " " <> show s
  show (HelpOutput    x s) = "HelpOutput " <> show x <> " " <> show s
  show (Output          s) = "Output " <> show s

runJS
  :: ∀ eff
   . Fn2
        Foreign
        Foreign
        (Eff (NeodocEff eff) Foreign)
runJS = mkFn2 go
  where
  go fSpec fOpts = do
    spec /\ opts <- either (throwException <<< error <<< F.prettyForeignError <<< NEL.head)
                      pure
                      (runExcept $ do
                        Tuple <$> (readSpec fSpec)
                              <*> (F.read fOpts))
    x <- _run spec opts
    pure case x of
      (Output x) -> F.toForeign (rawValue <$> x)
      (HelpOutput x s) ->
        let x' = StrMap.insert ".help" (StringValue s) x
         in F.toForeign (rawValue <$> x')
      (VersionOutput x s) ->
        let x' = StrMap.insert ".version" (StringValue s) x
         in F.toForeign (rawValue <$> x')

  -- | Convert a Value into a JS-native value.
  rawValue :: Value -> Unit
  rawValue (BoolValue   b) = unsafeCoerce b
  rawValue (IntValue    i) = unsafeCoerce i
  rawValue (FloatValue  x) = unsafeCoerce x
  rawValue (StringValue s) = unsafeCoerce s
  rawValue (ArrayValue xs) = unsafeCoerce $ rawValue <$> xs

readSpec
  :: Foreign
  -> F (Either (Spec UsageLayout) String)
readSpec input = do
  spec :: Either (Spec (EmptyableLayout UsageLayoutArg)) String <- do
    (Right <$> F.read input) <|>
    (Left  <$> F.read input)
  pure $ lmap Spec.fromEmptyableSpec spec

run
  :: ∀ eff
   . String
  -> NeodocOptions
  -> Eff (NeodocEff eff) Output
run help = _run (Right help)

run'
  :: ∀ eff
   . Spec UsageLayout
  -> NeodocOptions
  -> Eff (NeodocEff eff) Output
run' spec = _run (Left spec)

_run
  :: ∀ eff
   . Either (Spec UsageLayout) String
  -> NeodocOptions
  -> Eff (NeodocEff eff) Output
_run input (NeodocOptions opts) = do
  argv <- maybe (A.drop 2 <$> Process.argv) pure opts.argv
  env  <- maybe Process.getEnv              pure opts.env

  let
    runNeodocError' :: ∀ a. Either _ a -> Eff _ a
    runNeodocError' = runNeodocError Nothing Nothing Nothing

  -- 1. obtain a spec, either by using the provided spec or by parsing a fresh
  --    one.
  inputSpec@(Spec { program, helpText, shortHelp }) <- runNeodocError' do
    either pure parseHelpText input

  -- 2. solve the spec
  spec@(Spec { descriptions }) <- runNeodocError' do
    let fromJSCallback
          :: ∀ a
           . (Pretty a)
          => (Spec a -> Eff _ (Spec a))
          -> (Spec a -> Either _ (Spec a))
        fromJSCallback cb = \spec ->
          let result = unsafePerformEff (cb spec)
           in Right result
    Error.capture do
      Solver.solve'
        { smartOptions: opts.smartOptions
        , helpFlags: fromFoldable opts.helpFlags
        , versionFlags: fromFoldable opts.versionFlags
        }
        (fromFoldable $ either (fromJSCallback <$> _) id opts.transforms.presolve)
        (fromFoldable $ either (fromJSCallback <$> _) id opts.transforms.postsolve)
        inputSpec

  let
    runNeodocError' :: ∀ a. Either _ a -> Eff _ a
    runNeodocError' = runNeodocError  (Just program)
                                      (Just (pretty <$> opts.helpFlags))
                                      (Just shortHelp)

  -- 3. run the arg parser agains the spec and user input
  output <- runNeodocError' do
    ArgParseResult mBranch vs <- do
      Error.capture do
        ArgParser.run spec {
            optionsFirst:      opts.optionsFirst
          , stopAt:            opts.stopAt
          , requireFlags:      opts.requireFlags
          , laxPlacement:      opts.laxPlacement
          , repeatableOptions: opts.repeatableOptions
          , allowUnknown:      opts.allowUnknown
          , implicitNegatives: opts.implicitNegatives
          , helpFlags:         fromFoldable opts.helpFlags
          , versionFlags:      fromFoldable opts.versionFlags
          } env argv
    pure $ Evaluate.reduce env descriptions mBranch vs

  if output `has` (pretty <$> opts.helpFlags) then
    let helpText' = trimHelp helpText
     in if opts.dontExit
          then pure (HelpOutput output helpText')
          else Console.log helpText' *> Process.exit 0
    else
      if output `has` (pretty <$> opts.versionFlags) then do
        mVer <- maybe readPkgVersion (pure <<< pure) opts.version
        case mVer of
          Just ver ->
            if opts.dontExit
                then pure (VersionOutput output ver)
                else Console.log ver *> Process.exit 0
          Nothing -> runNeodocError' $ Left Error.VersionMissingError
    else pure (Output output)

  where

  runNeodocError
    :: ∀ eff a
     . Maybe String         -- the program name, if available
    -> Maybe (Array String) -- the flags that trigger --help
    -> Maybe String         -- the shortened usage text
    -> Either NeodocError a
    -> Eff (NeodocEff eff) a
  runNeodocError mProg mHelpFlags mShortHelp x = case x of
    Left err ->
      let msg = renderNeodocError mProg mHelpFlags mShortHelp err
       in if opts.dontExit
            then throwException $ jsError msg {}
            else
              let msg' = if Error.isDeveloperError err
                            then msg <> "\n" <> _DEVELOPER_ERROR_MESSAGE
                            else msg
              in Console.error msg' *> Process.exit 1
    Right x -> pure x

  readPkgVersion = readPkgVersionImpl Just Nothing


runPure
  :: String
  -> NeodocOptions
  -> Maybe String
  -> Either NeodocError Output
runPure help = _runPure (Right help)

runPure'
  :: Spec UsageLayout
  -> NeodocOptions
  -> Maybe String
  -> Either NeodocError Output
runPure' spec = _runPure (Left spec)

_runPure
  :: Either (Spec UsageLayout) String
  -> NeodocOptions
  -> Maybe String
  -> Either NeodocError Output
_runPure input (NeodocOptions opts) mVer = do
  let argv = fromMaybe [] opts.argv
      env  = fromMaybe StrMap.empty opts.env

  -- 1. obtain a spec, either by using the provided spec or by parsing a fresh
  --    one.
  inputSpec@(Spec { program, helpText }) <- do
    either pure parseHelpText input

  -- 2. solve the spec
  spec@(Spec { descriptions }) <- do
    Error.capture do
      Solver.solve { smartOptions: opts.smartOptions } inputSpec

  -- 3. run the arg parser agains the spec and user input
  ArgParseResult mBranch vs <- do
    Error.capture do
      ArgParser.run spec {
          optionsFirst:      opts.optionsFirst
        , stopAt:            opts.stopAt
        , requireFlags:      opts.requireFlags
        , laxPlacement:      opts.laxPlacement
        , repeatableOptions: opts.repeatableOptions
        , allowUnknown:      opts.allowUnknown
        , implicitNegatives: opts.implicitNegatives
        , helpFlags:         fromFoldable opts.helpFlags
        , versionFlags:      fromFoldable opts.versionFlags
        } env argv

  let output = Evaluate.reduce env descriptions mBranch vs

  if output `has` (pretty <$> opts.helpFlags)
    then pure (HelpOutput output (trimHelp helpText))
    else
      if output `has` (pretty <$> opts.versionFlags) then do
        case mVer of
          Just ver -> pure (VersionOutput output ver)
          Nothing ->  Left Error.VersionMissingError
      else pure (Output output)

parseHelpTextJS
  :: ∀ eff
   . Fn1
        String
        (Eff (NeodocEff eff) Foreign)
parseHelpTextJS = mkFn1 go
  where
  go help =
    case parseHelpText help of
      Left e ->
        let msg = renderNeodocError Nothing Nothing Nothing e
         in throwException $ jsError msg {}
      Right spec ->
        pure $ F.write $ Spec.toEmptyableSpec spec

parseHelpText
  :: String
  -> Either NeodocError (Spec UsageLayout)
parseHelpText help = do
  -- scan the input text
  { originalUsage, usage, options } <- Error.capture do
    Scanner.scan $ dedent help

  -- lex/parse the usage section
  { program, layouts } <- do
    toks <- Error.capture $ Lexer.lexUsage usage
    Error.capture $ Spec.parseUsage toks

  -- lex/parse the description section(s)
  descriptions <- concat <$> for options \description -> do
    toks <- Error.capture $ Lexer.lexDescs description
    Error.capture $ Spec.parseDescription toks

  pure $ Spec { program
              , layouts
              , descriptions
              , helpText: help
              , shortHelp: originalUsage
              }

renderNeodocError
  :: Maybe String         -- the program name, if available
  -> Maybe (Array String) -- the flags that trigger --help
  -> Maybe String         -- the shortened usage text
  -> NeodocError          -- the error that occured
  -> String
renderNeodocError (Just prog) mHelpFlags mShortHelp (Error.ArgParserError msg) =
  -- de-capitalize the error message after the colon
  let
    title =
      case String.uncons msg of
        Nothing -> msg
        Just { head, tail } ->
          let msg' = String.singleton (Char.toLower head) <> tail
           in prog <> ": " <> msg'
    usage = renderShortHelp mShortHelp
    help = renderHelpFlags prog mHelpFlags
   in title
        <> (if String.length title > 0 then "\n" else "")
        <> usage
        <> (if String.length usage > 0 then "\n" else "")
        <> help
  where
  renderShortHelp (Just help) =
    dedent $ unlines $ ("  " <> _) <$> lines (dedent help)
  renderShortHelp _ = ""
  renderHelpFlags prog (Just flags) | not (A.null flags) =
    "See " <> prog <> " " <> (intercalate "/" flags)
      <> " for more information"
  renderHelpFlags _ _ = ""
renderNeodocError _ _ _ e = pretty e

has x = any \s ->
  maybe false (case _ of
                IntValue  0     -> false
                BoolValue false -> false
                ArrayValue []   -> false
                _               -> true
                ) (StrMap.lookup s x)

trimHelp = Regex.replace (regex' "(^\\s*(\r\n|\n|\r))|((\r\n|\n|\r)\\s*$)" "g") ""
  where regex' a b = unsafePartial $ fromRight $ regex a (Regex.parseFlags b)

foreign import jsError :: ∀ a. String -> a -> Error
foreign import readPkgVersionImpl
  :: ∀ e
   . (String -> Maybe String)
  -> Maybe String
  -> Eff e (Maybe String)
