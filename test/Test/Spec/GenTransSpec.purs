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

import Language.Docopt
import Language.Docopt.ParserGen (genParser, runParser)
import qualified Language.Docopt.ParserGen.Trans as T

import Test.Assert (assert)
import Test.Spec (describe, it, Spec())
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff, prettyPrintMap)
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
              [ "i"     :> str "xuq"
              , "input" :> str "xuq"
              ]
        ]

    for_ tests \(Test test) -> do

      let inp = Map.fromList $ toList test.i
          exp = Map.fromList $ toList test.o
          out = T.byName inp

      describe (prettyPrintIn inp) do
        it (prettyPrintOut exp) do
          vliftEff do
            if exp == out
                then return unit
                else throwException $ error $
                      "Unexpected output:\n"
                        ++ prettyPrintOut out

  where
    pretty :: forall k. Map k Value -> (k -> String) -> String
    pretty m = flip (prettyPrintMap m) prettyPrintValue

    prettyPrintIn :: Map Argument Value -> String
    prettyPrintIn m = "\n\t" ++ (pretty m prettyPrintArg)

    prettyPrintOut :: Map String Value -> String
    prettyPrintOut m = "\n\t" ++ (pretty m show)
