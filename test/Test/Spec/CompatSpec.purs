module Test.Spec.CompatSpec (compatSpec) where

import Prelude
import Debug.Trace
import Debug.Profile
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, later)
import Control.Alt ((<|>))
import Data.StrMap as StrMap
import Data.Bifunctor (lmap)
import Data.Pretty (pretty)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..), either)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Foldable (intercalate, for_)
import Text.Wrap (dedent)
import Data.Maybe (Maybe(..), fromMaybe, fromJust, maybe)
import Data.Traversable (for)
import Data.List (List(..), many, toUnfoldable, concat)
import Test.Spec (Spec(), describe, it)
import Test.Spec (Spec()) as Test
import Data.String as String
import Test.Support (vliftEff)
import Partial.Unsafe (unsafePartial)
import Control.Apply ((*>), (<*))
import Node.FS (FS)

import Neodoc as Neodoc
import Neodoc (NeodocOptions(..))
import Neodoc.Value
import Neodoc.Env (Env)
import Neodoc.Env as Env
import Neodoc.Error.Class (capture) as Error
import Neodoc.Spec as Neodoc
import Neodoc.Spec (Spec(..))
import Neodoc.Spec.Parser as Spec
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Scanner as Scanner
import Neodoc.Solve as Solver
import Neodoc.Solve.Error (SolveError(..))
import Neodoc.Solve.ExpandOptions (expandOptions, ExpandedOptionsLayout(..)
, ExpandedOptionsLayoutArg(..))
import Neodoc.Data.SolvedLayout (SolvedLayout(..))
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.ArgParser as ArgParser
import Neodoc.ArgParser (ArgParseResult(..))
import Neodoc.Evaluate as Evaluate

import Test.Support.CompatParser

type CompatEff e = (fs :: FS, err :: EXCEPTION | e)

compatSpec :: ∀ e. List Test -> Test.Spec (CompatEff e) Unit
compatSpec tests =
  describe "Docopt compatibility" do
    for_ tests \(Test { doc, kases }) -> do
      describe (doc <> "\n") do
        for_ kases \(Kase { options: (NeodocOptions opts), out }) -> do
          let argv = unsafePartial $ fromJust opts.argv
              env  = unsafePartial $ fromJust opts.env
              flagsDesc = renderFlags { optionsFirst: opts.optionsFirst
                                      , smartOptions: opts.smartOptions
                                      , requireFlags: opts.requireFlags
                                      , laxPlacement: opts.laxPlacement
                                      , repeatableOptions: opts.repeatableOptions
                                      , allowUnknown: opts.allowUnknown
                                      , implicitNegatives: opts.implicitNegatives
                                      }
          describe (intercalate " " (
            (toUnfoldable $ StrMap.toList env <#> \t ->
                fst t <> "=\"" <> snd t <> "\"")
              <> argv
            )
            <> (if String.length flagsDesc > 0 then " # flags: " <> flagsDesc else "")
            ) do
            it ("\n" <> pretty out) do

              -- XXX: Manually break the execution context in order to avoid to
              --      avoid stack overflows by executing a large amount of Aff
              --      actions that run purely synchronous. Ideally, we would run
              --      the `Aff` action using it's `MonadRec` instance.
              -- Refer: https://github.com/owickstrom/purescript-spec/issues/24

              later (pure unit)

              let env = fromMaybe StrMap.empty opts.env

              result <- prof "testcase" \_-> pure $ Neodoc.runPure (dedent doc) (NeodocOptions opts) Nothing

              vliftEff $ case result of
                Left e ->
                  either
                    (\es ->
                      if es == "user-error" || es == "spec-error"
                        then pure unit
                        else if (pretty e {- XXX: Check against `ArgParserError`? -}) == es
                          then pure unit
                          else throwException $ error $
                            "Unexpected exception message: \"" <> pretty e <> "\""
                    )
                    (const $ throwException $ error $ pretty e)
                    out
                Right (Neodoc.Output output) -> do
                  either
                    (\_ -> do
                      throwException $ error $
                        "Unexpected output: \n"
                          <> pretty (StrMap.toList output)
                    )
                    (\expected ->
                      let actual = StrMap.toList output
                       in if (StrMap.fromFoldable expected /= output)
                        then throwException $ error $
                          "Unexpected output:\n"
                            <> pretty actual
                        else pure unit)
                    out
                Right x -> throwException $ error $
                              "Unexpected output:\n"
                                <> pretty x
