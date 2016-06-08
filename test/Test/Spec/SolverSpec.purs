module Test.Spec.SolverSpec (solverSpec) where

import Prelude
import Debug.Trace
import Data.Either (Either(..), either)
import Control.Bind ((=<<))
import Control.Apply ((*>))
import Data.List (List(..), toList)
import Control.Plus (empty)
import Data.Foldable (intercalate, for_)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromChar)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple

import Test.Support (vliftEff, runEitherEff)
import Test.Support.Usage as U
import Test.Support.Desc as DE
import Test.Support.Docopt as D
import Test.Support.Arguments

import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.Solver (solve)
import Language.Docopt.Parser.Desc (Desc())
import Language.Docopt.Argument as D
import Test.Support.Docopt      as D
import Language.Docopt.Parser.Usage.Argument as U
import Language.Docopt.Parser.Usage          as U
import Language.Docopt.Parser.Desc           as DE
import Language.Docopt.Scanner (scan)
import Language.Docopt.Parser.Lexer (lex)
import Text.Wrap (dedent)

newtype TestSuite = TestSuite { usages :: Array U.Usage
                              , cases  :: Array TestCase
                              }
newtype TestCase = TestCase { descs    :: Array Desc
                            , expected :: Either String (Array Usage) }

test :: Array U.Usage -> Array TestCase -> TestSuite
test us cs = TestSuite { usages: us, cases: cs }

pass :: Array Desc -> Array Usage -> TestCase
pass ds as = TestCase { descs: ds, expected: Right as }

fail :: Array Desc -> String -> TestCase
fail ds msg = TestCase { descs: ds, expected: Left msg }

out :: Array (Array Argument) -> Usage
out xss = Usage $ toList $ toList <$> xss

u = U.usage "foo"

solverSpec = \_ ->
  describe "The solver" do
    (flip traverseWithIndex_) (toList [

      test ([ u [ [ U.co "foo" ] ] ])
        [ pass  ([])
                ([ out [ [ co "foo" ] ] ])
        ]

    , test ([ u [ [ U.poR "foo" ] ] ])
        [ pass  ([])
                ([ out [ [ poR "foo" ] ] ])
        ]

    , test ([ u [ [ U.loptR_ "foo" ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "foo")
                            (Just $ DE.arg "bar" false (Just (StringValue "qux")))
                ])
                ([ out [
                    [ optR 'f' "foo" (oa "bar" (StringValue "qux")) ]
                ] ])
        ]

    , test ([ u [ [ U.loptR_ "foo", U.co "BAR" ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "foo")
                            (Just $ DE.arg "BAR" false (Just (StringValue "qux")))
                ])
                ([ out [
                    [ optR 'f' "foo" (oa "BAR" (StringValue "qux"))
                    , co "BAR"
                    ]
                ] ])
        ]

    , test ([ u [ [ U.lopt_ "foo", U.poR "BAR" ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "foo")
                            (Just $ DE.arg "BAR" false (Just (StringValue "qux")))
                ])
                ([ out [
                    [ optR 'f' "foo" (oa "BAR" (StringValue "qux")) ]
                ] ])
        ]

    , test ([ u [ [ U.lopt_ "foo", U.po "BAR" ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "foo")
                            (Just $ DE.arg "BAR" false (Just (StringValue "qux")))
                ])
                ([ out [
                    [ opt 'f' "foo" (oa "BAR" (StringValue "qux")) ]
                ] ])
        ]

    , test ([ u [ [ U.loptR_ "foo", U.poR "BAR" ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "foo")
                          (Just $ DE.arg "BAR" false (Just (StringValue "qux")))
                ])
                ([ out [
                    [ optR 'f' "foo" (oa "BAR" (StringValue "qux"))
                    , poR "BAR"
                    ]
                ] ])
        ]

    , test ([ u [ [ U.soptR_ 'x' ['v', 'z', 'f'] ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "file")
                            (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ])
                ([ out [
                    [ soptR_ 'x'
                    , soptR_ 'v'
                    , soptR_ 'z'
                    , optR 'f' "file" (oa "FILE" (StringValue "foo"))
                    ]
                ] ])
        ]

    , test ([ u [ [ U.lopt_ "file", U.poR "FILE" ] ] ])
        [ fail  [ DE.opt (DE.fname 'f' "file")
                           (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                , DE.opt (DE.fname 'f' "file")
                           (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ]
                "Multiple option descriptions for option --file"
        ]

    , test ([ u [ [ U.sopt_ 'f' [], U.poR "FILE" ] ] ])
        [ fail  [ DE.opt (DE.fname 'f' "file")
                           (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                , DE.opt (DE.fname 'f' "file")
                           (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ]
                "Multiple option descriptions for option -f"
        ]

    , test ([ u [ [ U.lopt_ "file", U.poR "FILE" ] ] ])
        [ fail  [ DE.opt (DE.fname 'f' "file")
                           (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                , DE.opt (DE.fname 'f' "file")
                           (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ]
                "Multiple option descriptions for option --file"
        ]

    , test ([ u [ [ U.soptR_ 'f' ['x'] ] ] ])
        [ fail  ([ DE.opt (DE.fname 'f' "file")
                            (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ])
                "Stacked option -f may not specify arguments"
        ]

      -- Note: `f` should not adopt `file` as it's full name since it's in an
      -- option stack and not in trailing position (therefore cannot inherit the
      -- description's argument, rendering it an unfit candidate)
    , test ([ u [ [ U.soptR_ 'f' ['v', 'z', 'x'] ] ] ])
        [ fail  ([ DE.opt (DE.fname 'f' "file")
                            (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ])
                "Stacked option -f may not specify arguments"
        ]

    , test ([ u [ [ U.soptR_ 'x' ['v', 'z', 'f'] ] ] ])
        [ pass  ([ DE.opt (DE.fname 'f' "file")
                            (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
                ])
                ([ out [
                    [ soptR_ 'x'
                    , soptR_ 'v'
                    , soptR_ 'z'
                    , optR 'f' "file" (oa "FILE" (StringValue "foo"))
                    ]
                ] ])
        ]

    , test ([ u [ [ U.Reference "" ] ] ])
        [ pass
            [ DE.opt (DE.fname 'f' "file")
                     (Just $ DE.arg "FILE" false (Just (StringValue "foo")))
            ]
            [ out [ [ gro false [ [
                opt 'f' "file" (oa "FILE" $ StringValue "foo")
            ] ] ] ] ]
        ]
    ]) runtest

  where

    prettyPrintOutput :: List Usage -> String
    prettyPrintOutput as =
      intercalate "\n" (("  " ++ ) <$>
        (prettyPrintUsage <$> as))

    runtest n (TestSuite { usages, cases }) = do
      describe
        ("\nUsage:\n" ++ intercalate "\n" (("  " ++) <$>
          (U.prettyPrintUsage <$> usages))) do
        (flip traverseWithIndex_) (toList cases)
          \j (TestCase { descs, expected }) -> do
            describe
              ("\nOptions:\n" ++ intercalate "\n" (("  " ++) <$>
                (DE.prettyPrintDesc <$> descs))) do
              it (
                either
                  (\msg -> "Should fail with:\n" ++ msg)
                  (\as  -> "Should resolve to:\n" ++ prettyPrintOutput (toList as))
                  expected
              ) do
                vliftEff do
                  evaltest
                    (solve (toList usages) (toList descs))
                    (toList <$> expected)

    evaltest (Right output) (Right expected)
      = if output == (toList expected)
            then return unit
            else throwException $ error $
              "Unexpected output:\n" ++ prettyPrintOutput output

    evaltest (Right output) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n" ++ prettyPrintOutput output

    evaltest (Left (SolveError err)) (Left expected)
      = if err == expected
            then return unit
            else throwException $ error $
              "Unexpected error:\n" ++ err

    evaltest (Left err) _ = throwException $ error $ show err

    traverseWithIndex_ :: forall a b m. (Applicative m) => (Int -> a -> m b)
                                                        -> (List a)
                                                        -> m Unit
    traverseWithIndex_ f xs = go xs 0
        where go Nil _         = return unit
              go (Cons x xs) i = f i x *> go xs (i + 1)
