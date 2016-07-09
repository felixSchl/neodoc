module Test.Spec.CompatSpec (compatSpec) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, later)
import Control.Alt ((<|>))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..), either)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Foldable (intercalate, for_)
import Text.Wrap (dedent)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.List (List, many, toUnfoldable)
import Test.Spec (Spec(), describe, it)
import Data.String as String
import Test.Support (vliftEff, runEitherEff)
import Partial.Unsafe (unsafePartial)
import Control.Apply ((*>), (<*))
import Node.FS (FS)

import Docopt as Docopt
import Language.Docopt (runDocopt)
import Language.Docopt.Value (Value(..), prettyPrintValue) as D
import Test.Support.CompatParser

type CompatEff e = (fs :: FS, err :: EXCEPTION | e)

compatSpec :: forall e. List Test -> Spec (CompatEff e) Unit
compatSpec tests =
  describe "Docopt compatibility" do
    for_ tests \(Test { doc, kases }) -> do
      describe (doc <> "\n") do
        for_ kases \(Kase { options, out }) -> do
          let argv = unsafePartial $ fromJust options.argv
              env  = unsafePartial $ fromJust options.env
              flagsDesc = renderFlags { optionsFirst: options.optionsFirst
                                      , smartOptions: options.smartOptions
                                      }
          describe (intercalate " " (
            (toUnfoldable $ StrMap.toList env <#> \t ->
                fst t <> "=\"" <> snd t <> "\"")
              <> argv
            )
            <> (if String.length flagsDesc > 0 then " # flags: " <> flagsDesc else "")
            ) do
            it ("\n" <> prettyPrintOut out) do

              -- XXX: Manually break the execution context in order to avoid to
              --      avoid stack overflows by executing a large amount of Aff
              --      actions that run purely synchronous. Ideally, we would run
              --      the `Aff` action using it's `MonadRec` instance.
              -- Refer: https://github.com/owickstrom/purescript-spec/issues/24

              later (pure unit)

              let result = runDocopt (dedent doc)
                                     (fromMaybe StrMap.empty options.env)
                                     argv
                                     options
              vliftEff $ case result of
                Left e ->
                  either
                    (\es ->
                      if es == "user-error" || es == "spec-error"
                        then pure unit
                        else if e == es
                          then pure unit
                          else throwException $ error $
                            "Unexpected exception message: \"" <> e <> "\""
                    )
                    (const $ throwException $ error $ e)
                    out
                Right output -> do
                  either
                    (\_ -> do
                      throwException $ error $
                        "Unexpected output: \n"
                          <> prettyPrintOut (pure $ StrMap.toList output)
                    )
                    (\expected ->
                      let actual = StrMap.toList output
                       in if (StrMap.fromFoldable expected /= output)
                        then throwException $ error $
                          "Unexpected output:\n"
                            <> prettyPrintOut (pure actual)
                        else pure unit)
                    out

  where
    prettyPrintOut :: Either String (List (Tuple String D.Value)) -> String
    prettyPrintOut (Left "user-error") = "fail at parsing argv"
    prettyPrintOut (Left "spec-error") = "fail at parsing spec"
    prettyPrintOut (Left err) = "fail with: \"" <> err <> "\""
    prettyPrintOut (Right xs)
      = intercalate "\n" $ xs <#> \(Tuple k v) -> k <> " => " <> show v
