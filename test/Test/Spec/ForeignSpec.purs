module Test.Spec.ForeignSpec (foreignSpec) where

import Prelude
import Debug.Trace
import Data.Foreign (toForeign)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, later)
import Control.Alt ((<|>))
import Data.StrMap as StrMap
import Data.Function.Uncurried
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Foldable (intercalate, for_)
import Text.Wrap (dedent)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.List (List(..), (:), many, toUnfoldable)
import Test.Spec (Spec(), describe, it)
import Data.String as String
import Test.Support (vliftEff, runEitherEff)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Control.Apply ((*>), (<*))
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Data.Array as A
import Data.Array.Partial as AU
import Node.FS (FS)

import Docopt as Docopt
import Docopt.FFI as DocoptFFI
import Language.Docopt (runDocopt, Docopt)
import Language.Docopt.Specification (prettyPrintSpec)
import Language.Docopt.Value (Value(..), prettyPrintValue) as D
import Test.Support.CompatParser

type CompatEff e = (fs :: FS, err :: EXCEPTION | e)

-- XXX: Provisionary wrapper to test equality etc.
data DocoptWrapper = DocoptWrapper Docopt
instance eqDocoptWrapper :: Eq DocoptWrapper where
  eq (DocoptWrapper a) (DocoptWrapper b) =
    a.shortHelp == b.shortHelp &&
    a.specification == b.specification

foreignSpec :: forall e. List Test -> Spec (CompatEff e) Unit
foreignSpec tests = describe "Crossing JS/purescript" do
  for_ tests \(Test { doc, kases }) -> do
    if any (\(Kase k) -> case k.out of
      Left e | e == "spec-error" -> true -- skip
      Left  _ -> false
      Right _ -> false
    ) kases
      then pure unit
      else
        it doc do

          -- XXX: Manually break the execution context in order to avoid to
          --      avoid stack overflows by executing a large amount of Aff
          --      actions that run purely synchronous. Ideally, we would run
          --      the `Aff` action using it's `MonadRec` instance.
          -- Refer: https://github.com/owickstrom/purescript-spec/issues/24

          later (pure unit)

          vliftEff do
            let helpText = dedent doc
            -- parse the spec as a purescript value
            expected <- unsafeInterleaveEff do
              Docopt.parse helpText {
                smartOptions: true
              }

            -- parse the spec as a JS value
            -- we could use `specToForeign` on the ouput above, but this -
            -- albeit slower - gives more confidence that everything's
            -- alright.
            input <- unsafeInterleaveEff do
              runFn2
                DocoptFFI.parse helpText (toForeign {
                  smartOptions: true
                })

            let result = DocoptFFI.readSpec (toForeign input)

            case result of
              Left e -> throwException $ error $ show e
              Right output -> do
                if DocoptWrapper output /= DocoptWrapper expected
                  -- XXX: Would be cool to get some sort of diffing in here.
                  then throwException $ error $
                    "input and output mismatches:\n"
                      <> "Expected:\n"
                      <> prettyPrintSpec expected.specification
                      <> "\n"
                      <> "Received:\n"
                      <> prettyPrintSpec output.specification
                  else pure unit
