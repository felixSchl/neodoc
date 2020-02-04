module Neodoc
  ( runString
  , runStringJs
  , runSpec
  , runSpecJs
  , parseHelpText
  , lookup
  , lookup'
  , Output (..)
  , module Options
  )
where

import Prelude
  (class Ord, class Show, bind, not, pure, show, ($), (<$>), (<>), (>))

import Data.Argonaut.Core
  (Json, jsonNull, jsonSingletonArray, jsonSingletonObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as A
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (class Foldable, any, intercalate)
import Data.List (concat, fromFoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Pretty (class Pretty, pretty)
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Yarn (lines, unlines)
import Data.Traversable (for)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Text.Wrap (dedent)

import Neodoc.ArgKey (ArgKey, argKeyMapToString, stringMapToArgKey)
import Neodoc.ArgParser (ArgParseResult(..))
import Neodoc.ArgParser as ArgParser
import Neodoc.Data.UsageLayout (UsageLayout)
import Neodoc.Error (NeodocError(..)) as Error
import Neodoc.Error (NeodocError)
import Neodoc.Error.Class (capture) as Error
import Neodoc.Evaluate as Evaluate
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Options
  (Argv, NeodocOptions(..), customize, defaultOptions, defaultOptionsObj)
  as Options
import Neodoc.Options (NeodocOptions(..))
import Neodoc.Scanner as Scanner
import Neodoc.Solve as Solver
import Neodoc.Spec (Spec(..))
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Spec.Parser (parseDescription, parseUsage) as Spec
import Neodoc.Value (Value(..))

foreign import readPkgVersionImpl
  :: (String -> Maybe String)
  -> Maybe String
  -> Effect (Maybe String)


data Output
  = VersionOutput (Map ArgKey Value) String
  | Output        (Map ArgKey Value)
  | HelpOutput    (Map ArgKey Value) String


getArgs :: Output -> Map String Value
getArgs (VersionOutput x _) = argKeyMapToString x
getArgs (Output          x) = argKeyMapToString x
getArgs (HelpOutput    x _) = argKeyMapToString x


lookup :: String -> Output -> Maybe Value
lookup k o = Map.lookup k (getArgs o)


lookup' :: String -> Output -> Either String Value
lookup' k o = case Map.lookup k (getArgs o) of
                  Just v -> Right v
                  _ -> case OptionAlias.fromString k of
                              Right _ -> Right (BoolValue false)
                              _       -> Left $ "no such key: " <> show k


instance prettyOutput :: Pretty Output where
  pretty (VersionOutput _ s) = s
  pretty (HelpOutput    _ s) = s
  pretty (Output          s) = show s


instance showOutput :: Show Output where
  show (VersionOutput x s) = "VersionOutput " <> show x <> " " <> show s
  show (HelpOutput    x s) = "HelpOutput " <> show x <> " " <> show s
  show (Output          s) = "Output " <> show s


runStringJs :: String -> Json -> Json
runStringJs helpStr opts =
  let
    jsonResult = do
      NeodocOptions neodocOptsObj <- decodeJson opts

      let outputResult = runString
            helpStr
            (NeodocOptions neodocOptsObj)
            neodocOptsObj.version

      case outputResult of
        Left neodocError -> Right $ encodeJson neodocError
        Right val -> Right $ encodeJson $ case val of
          (Output x) -> argKeyMapToString x
          (HelpOutput x s) ->
            Map.insert ".help" (StringValue s) (argKeyMapToString x)
          (VersionOutput x s) ->
            Map.insert ".version" (StringValue s) (argKeyMapToString x)
  in
    case jsonResult of
      Left err -> jsonSingletonObject "errors" $
        jsonSingletonArray $ encodeJson err
      Right val -> val


runSpecJs :: Json -> Json -> Json
runSpecJs helpStr opts = jsonNull


runString
  :: String
  -> NeodocOptions
  -> Maybe String
  -> Either NeodocError Output
runString help = _runPure (Right help)

runSpec
  :: Spec UsageLayout
  -> NeodocOptions
  -> Maybe String
  -> Either NeodocError Output
runSpec spec = _runPure (Left spec)

_runPure
  :: Either (Spec UsageLayout) String
  -> NeodocOptions
  -> Maybe String
  -> Either NeodocError Output
_runPure input (NeodocOptions opts) mVer = do
  let argv = fromMaybe [] opts.argv
      env  = fromMaybe Map.empty opts.env

  -- 1. obtain a spec, either by using the provided spec or by parsing a fresh
  --    one.
  inputSpec@(Spec { program, helpText }) <- do
    either pure parseHelpText input

  -- 2. solve the spec
  spec@(Spec { descriptions }) <- do
    Error.capture do
      Solver.solve { smartOptions: opts.smartOptions } inputSpec

  -- 3. run the arg parser against the spec and user input
  ArgParseResult mBranch vs <- do
    Error.capture do
      let
        argOpts =
          { optionsFirst:      opts.optionsFirst
          , stopAt:            opts.stopAt
          , requireFlags:      opts.requireFlags
          , laxPlacement:      opts.laxPlacement
          , repeatableOptions: opts.repeatableOptions
          , allowUnknown:      opts.allowUnknown
          , helpFlags:         fromFoldable opts.helpFlags
          , versionFlags:      fromFoldable opts.versionFlags
          }

      ArgParser.run spec argOpts env argv

  let output = Evaluate.reduce env descriptions mBranch vs

  if output `has` (pretty <$> opts.helpFlags)
  then pure (HelpOutput (stringMapToArgKey output) (trimHelp helpText))
  else
    if output `has` (pretty <$> opts.versionFlags)
    then do
      case mVer of
        Just ver -> pure (VersionOutput (stringMapToArgKey output) ver)
        Nothing ->  Left Error.VersionMissingError
    else pure (Output (stringMapToArgKey output))


parseHelpText :: String -> Either NeodocError (Spec UsageLayout)
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
  -> NeodocError          -- the error that occurred
  -> String
renderNeodocError (Just prog) mHelpFlags mShortHelp (Error.ArgParserError msg) =
  -- de-capitalize the error message after the colon
  let
    title =
      case String.uncons msg of
        Nothing -> msg
        Just { head, tail } ->
          let msg' = String.toLower (String.singleton head) <> tail
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
  renderHelpFlags prog_ (Just flags) | not (A.null flags) =
    "See " <> prog_ <> " " <> (intercalate "/" flags)
      <> " for more information"
  renderHelpFlags _ _ = ""
renderNeodocError _ _ _ e = pretty e


has :: forall a b. Foldable a => Ord b => Map b Value -> a b -> Boolean
has x =
  any \s ->
    maybe false (case _ of
      IntValue  0     -> false
      BoolValue false -> false
      ArrayValue []   -> false
      _               -> true
      ) (Map.lookup s x)


trimHelp :: String -> String
trimHelp =
  let
    regex' a b = unsafePartial $ fromRight $ regex a (Regex.parseFlags b)
  in
    Regex.replace
      (regex' "(^\\s*(\r\n|\n|\r))|((\r\n|\n|\r)\\s*$)" "g")
      ""
