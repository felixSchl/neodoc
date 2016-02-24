module Test.Spec.GenTransSpec (genTransSpec) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.List (List(..), toList, length, fromList, singleton)
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Array as A
import Data.Foldable (for_, intercalate)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Text.Parsing.Parser as P

import Docopt
import Docopt.Gen (genParser, runParser)
import qualified Docopt.Gen.Trans as Trans

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)
import Test.Support.Docopt

(:>) = Tuple
infixr 0 :>

newtype Test = Test {
  i :: Array (Tuple Argument Value)
, o :: Array (Tuple String Value)
}

shouldBecome :: Array (Tuple Argument Value)
             -> Array (Tuple String Value)
             -> Test
shouldBecome i o = Test { i: i, o: o }

genTransSpec = \_ ->
  describe "The output transformer" do

    let tests = [
          [ opt  'i' "input" (oa "BAR" $ str "Foo") :> str "qux"
          , optR 'i' "input" (oa_ "BAR")            :> str "xuq" ]
            `shouldBecome`
              [ "i"     :> array [ str "qux", str "xuq" ]
              , "input" :> array [ str "qux", str "xuq" ] ]
        ]

    for_ tests \(Test test) -> do

      let inp = Map.fromList $ toList test.i
          exp = Map.fromList $ toList test.o
          out = Trans.transform inp

      describe (prettyPrintIn inp) do
        it (prettyPrintOut exp) do
          vliftEff do
            if exp == out
                then return unit
                else throwException $ error $
                      "Unexpected output:\n"
                        ++ prettyPrintOut out

  where
    prettyPrintIn :: Map Argument Value -> String
    prettyPrintIn m = "\n\t" ++ (prettyPrintMap m prettyPrintArg)

    prettyPrintOut :: Map String Value -> String
    prettyPrintOut m = "\n\t" ++ (prettyPrintMap m show)

    prettyPrintMap :: forall a. Map a Value -> (a -> String) -> String
    prettyPrintMap m p =
      let xs = Map.toList m
       in if length xs == 0
              then "{}"
              else "{ "
                ++ (intercalate "\n\t, " $
                      xs <#> \(Tuple arg val) ->
                        p arg ++ " => " ++ prettyPrintValue val)
                ++ "\n\t}"
