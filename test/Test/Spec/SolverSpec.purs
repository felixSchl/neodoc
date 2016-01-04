module Test.Spec.SolverSpec (solverSpec) where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Control.Bind ((=<<))
import Control.Apply ((*>))
import Data.List (List(..), toList)
import Control.Plus (empty)
import Data.Foldable (intercalate, for_)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Data.Array as A

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runEitherEff)
import qualified Test.Support.Usage as U
import qualified Test.Support.Docopt as D

import Docopt
import Docopt.Spec.Solver (solve, SolveError(..))
import qualified Docopt.Spec.Parser.Usage as U
import qualified Docopt.Spec.Parser.Desc as D
import Docopt.Spec.Parser.Scanner (scan)
import Docopt.Spec.Parser.Lexer (lex)
import Text.Wrap (dedent)

newtype TestSuite = TestSuite { usages :: Array U.Usage
                              , cases  :: Array TestCase
                              }
newtype TestCase = TestCase { descs    :: Array D.Desc
                            , expected :: Either String (Array Application) }

test :: Array U.Usage -> Array TestCase -> TestSuite
test us cs = TestSuite { usages: us, cases: cs }

pass :: Array D.Desc -> Array Application -> TestCase
pass ds as = TestCase { descs: ds, expected: Right as }

fail :: Array D.Desc -> String -> TestCase
fail ds msg = TestCase { descs: ds, expected: Left msg }

usage :: Array (Array U.Argument) -> U.Usage
usage = U.usage "foo"

application :: Array (Array Argument) -> Application
application xss = Application $ toList $ (\xs -> Branch $ toList xs) <$> xss

solverSpec =
  describe "solver" do
    for_ [
      test ([ usage [ [ U.co "foo" ] ] ])
        [ pass  ([])
                ([ application [ [ D.co "foo" ] ] ])
        ]
    ] runtest
  where
    runtest (TestSuite { usages, cases }) = do
      (flip traverseWithIndex_) (toList cases)
        \j (TestCase { descs, expected }) -> do
          it ("case " ++ show (j + 1) ++ "/" ++ (show $ A.length cases)) do
            vliftEff do
              evaltest
                (solve (toList usages) (toList descs))
                (toList <$> expected)

    evaltest (Right output) (Right expected)
      = if output == (toList expected)
            then return unit
            else throwException $ error $
              "Unexpected output:\n" ++ show output

    evaltest (Right output) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n" ++ show output

    evaltest (Left err) (Left expected)
      = if (show err) == expected
            then return unit
            else throwException $ error $
              "Unexpected error:\n" ++ show err

    evaltest (Left err) _ = throwException $ error $ show err

    traverseWithIndex_ :: forall a b m. (Applicative m) => (Int -> a -> m b)
                                                        -> (List a)
                                                        -> m Unit
    traverseWithIndex_ f xs = go xs 0
        where go Nil _         = return unit
              go (Cons x xs) i = f i x *> go xs (i + 1)
