module Neodoc
  -- ( runString
  -- , runSpec
  -- ,
  (runPure
  , runPure'
  , parseHelpText
  -- , readSpec
  , lookup
  , lookup'
  , Output (..)
  , module Reexports
) where

import Prelude
import Debug.Profile
import Data.Array as A
import Data.Generic.Rep
import Data.Newtype (unwrap)
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Function.Uncurried
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either (..), either, fromRight)
import Data.String as String
import Data.Char as Char
import Data.List
  ( List(..), (:), many, toUnfoldable, concat
  , fromFoldable, catMaybes, filter, length
  )
import Data.List.NonEmpty as NEL
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (for)
import Data.Pretty (class Pretty, pretty)
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Foreign (F, Foreign)
import Foreign as F
import Foreign.Object as FObj
import Data.Foldable (any, intercalate)
import Data.String.Yarn (lines, unlines)
import Control.Alt ((<|>))
import Effect
import Effect.Exception (Error, throwException, error)
-- import Effect.Console (CONSOLE)
import Effect.Console as Console
-- import Effect.Class (liftEff)
-- import Effect (Eff)
import Effect.Unsafe
import Control.Monad.Except (throwError, catchError, runExcept)
import Text.Wrap (dedent)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)
import Node.Process as Process

import Neodoc.ArgKey
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


-- foreign import jsError :: ∀ a. String -> a -> Error

foreign import readPkgVersionImpl
  :: (String -> Maybe String)
  -> Maybe String
  -> Effect (Maybe String)


_DEVELOPER_ERROR_MESSAGE :: String
_DEVELOPER_ERROR_MESSAGE = dedent """
  This is an error with the program itself and not your fault.
  Please bring this to the program author's attention.
"""


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


-- runString :: String -> JSON -> (Effect Output)
-- runString spec opts = do
--   x <- _run spec opts
--   pure case x of
--     (Output x) -> F.toForeign (rawValue <$> x)
--     (HelpOutput x s) ->
--       let x' = Map.insert ".help" (StringValue s) x
--        in F.toForeign (rawValue <$> x')
--     (VersionOutput x s) ->
--       let x' = Map.insert ".version" (StringValue s) x
--        in F.toForeign (rawValue <$> x')



-- readSpec :: Foreign -> F (Either (Spec UsageLayout) String)
-- readSpec input = do
--   spec :: Either (Spec (EmptyableLayout UsageLayoutArg)) String <-
--     ((Right <$> F.read input) <|>
--         (Left  <$> F.read input))
--   pure $ lmap Spec.fromEmptyableSpec spec


-- runString :: String -> NeodocOptions -> Effect Output
-- runString help = _run (Right help)


-- runSpec :: Spec UsageLayout -> NeodocOptions -> Effect Output
-- runSpec spec = _run (Left spec)


-- _run
--   :: Either (Spec UsageLayout) String
--   -> NeodocOptions
--   -> Effect Output
-- _run input (NeodocOptions opts) = do
--   argv <- maybe
--     (A.drop 2 <$> Process.argv)
--     pure opts.argv
--   env  <-
--     -- TODO
--     -- maybe
--     -- (Map.fromFoldable $ FObj.toUnfoldable Process.getEnv)
--     pure opts.env

--   let
--     runNeodocError' :: ∀ a. Either _ a -> Effect a
--     runNeodocError' = runNeodocError Nothing Nothing Nothing

--   -- 1. obtain a spec, either by using the provided spec or by parsing a fresh
--   --    one.
--   inputSpec@(Spec { program, helpText, shortHelp }) <- runNeodocError' do
--     either pure parseHelpText input

--   -- 2. solve the spec
--   spec@(Spec { descriptions }) <- runNeodocError' do
--     let fromJSCallback
--           :: ∀ a
--            . (Pretty a)
--           => (Spec a -> Effect (Spec a))
--           -> (Spec a -> Either _ (Spec a))
--         fromJSCallback cb = \spec ->
--           let result = unsafePerformEffect (cb spec)
--            in Right result
--     Error.capture do
--       Solver.solve'
--         { smartOptions: opts.smartOptions
--         , helpFlags: fromFoldable opts.helpFlags
--         , versionFlags: fromFoldable opts.versionFlags
--         }
--         (fromFoldable $ either (fromJSCallback <$> _) identity opts.transforms.presolve)
--         (fromFoldable $ either (fromJSCallback <$> _) identity opts.transforms.postsolve)
--         inputSpec

--   let
--     runNeodocError' :: ∀ a. Either _ a -> Effect _ a
--     runNeodocError' = runNeodocError  (Just program)
--                                       (Just (pretty <$> opts.helpFlags))
--                                       (Just shortHelp)

--   -- 3. run the arg parser agains the spec and user input
--   output <- runNeodocError' do
--     ArgParseResult mBranch vs <- do
--       Error.capture do
--         ArgParser.run spec {
--             optionsFirst:      opts.optionsFirst
--           , stopAt:            opts.stopAt
--           , requireFlags:      opts.requireFlags
--           , laxPlacement:      opts.laxPlacement
--           , repeatableOptions: opts.repeatableOptions
--           , allowUnknown:      opts.allowUnknown
--           , helpFlags:         fromFoldable opts.helpFlags
--           , versionFlags:      fromFoldable opts.versionFlags
--           } env argv
--     pure $ Evaluate.reduce env descriptions mBranch vs

--   if output `has` (pretty <$> opts.helpFlags) then
--     let helpText' = trimHelp helpText
--      in if opts.dontExit
--           then pure (HelpOutput output helpText')
--           else Console.log helpText' *> Process.exit 0
--     else
--       if output `has` (pretty <$> opts.versionFlags) then do
--         mVer <- maybe readPkgVersion (pure <<< pure) opts.version
--         case mVer of
--           Just ver ->
--             if opts.dontExit
--                 then pure (VersionOutput output ver)
--                 else Console.log ver *> Process.exit 0
--           Nothing -> runNeodocError' $ Left Error.VersionMissingError
--     else pure (Output output)

--   where

--   runNeodocError
--     :: forall a. Maybe String         -- the program name, if available
--     -> Maybe (Array String) -- the flags that trigger --help
--     -> Maybe String         -- the shortened usage text
--     -> Either NeodocError a
--     -> Effect a
--   runNeodocError mProg mHelpFlags mShortHelp x = case x of
--     Left err ->
--       let msg = renderNeodocError mProg mHelpFlags mShortHelp err
--        in if opts.dontExit
--             then throwException $ error msg
--             else
--               let msg' = if Error.isDeveloperError err
--                             then msg <> "\n" <> _DEVELOPER_ERROR_MESSAGE
--                             else msg
--               in Console.error msg' *> Process.exit 1
--     Right x -> pure x

--   readPkgVersion = readPkgVersionImpl Just Nothing


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
      env  = fromMaybe Map.empty opts.env

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
  -> NeodocError          -- the error that occured
  -> String
renderNeodocError _ _ _ e = pretty e
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
  renderHelpFlags prog (Just flags) | not (A.null flags) =
    "See " <> prog <> " " <> (intercalate "/" flags)
      <> " for more information"
  renderHelpFlags _ _ = ""


has :: forall a b c. Map a Value -> b c -> Boolean
has x y = true
  -- TODO
  -- any \s ->
  --   maybe false (case _ of
  --     IntValue  0     -> false
  --     BoolValue false -> false
  --     ArrayValue []   -> false
  --     _               -> true
  --     ) (Map.lookup s x)


trimHelp :: _
trimHelp =
  let
    regex' a b = unsafePartial $ fromRight $ regex a (Regex.parseFlags b)
  in
    Regex.replace
      (regex' "(^\\s*(\r\n|\n|\r))|((\r\n|\n|\r)\\s*$)" "g")
      ""
