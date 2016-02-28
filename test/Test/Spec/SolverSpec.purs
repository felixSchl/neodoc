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
import qualified Test.Support.Usage as U
import qualified Test.Support.Docopt as D
import qualified Test.Support.Desc as Desc

import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Value
import Language.Docopt.Usage
import Language.Docopt.Solver (solve)
import Language.Docopt.Parser.Desc (Desc())
import qualified Language.Docopt.Parser.Usage.Argument as U
import qualified Language.Docopt.Parser.Usage          as U
import qualified Language.Docopt.Parser.Desc           as Desc
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
out xss = Usage $ toList $ (\xs -> Branch $ toList xs) <$> xss

solverSpec = \_ ->
  describe "The solver" do
    (flip traverseWithIndex_) (toList [

      test ([ U.usage "foo" [ [ U.co "foo" ] ] ])
        [ pass  ([])
                ([ out [ [ D.co "foo" ] ] ])
        ]

    , test ([ U.usage "foo" [ [ U.poR "foo" ] ] ])
        [ pass  ([])
                ([ out [ [ D.poR "foo" ] ] ])
        ]

    , test ([ U.usage "foo" [ [ U.loptR_ "foo" ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "foo")
                            (Just $ Desc.arg "bar" (Just "qux"))
                ])
                ([ out [
                    [ D.optR 'f' "foo" (D.oa "bar" (StringValue "qux")) ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.loptR_ "foo", U.co "BAR" ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "foo")
                            (Just $ Desc.arg "BAR" (Just "qux"))
                ])
                ([ out [
                    [ D.optR 'f' "foo" (D.oa "BAR" (StringValue "qux"))
                    , D.co "BAR"
                    ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.lopt_ "foo", U.poR "BAR" ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "foo")
                            (Just $ Desc.arg "BAR" (Just "qux"))
                ])
                ([ out [
                    [ D.optR 'f' "foo" (D.oa "BAR" (StringValue "qux")) ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.lopt_ "foo", U.po "BAR" ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "foo")
                            (Just $ Desc.arg "BAR" (Just "qux"))
                ])
                ([ out [
                    [ D.opt 'f' "foo" (D.oa "BAR" (StringValue "qux")) ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.loptR_ "foo", U.poR "BAR" ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "foo")
                            (Just $ Desc.arg "BAR" (Just "qux"))
                ])
                ([ out [
                    [ D.optR 'f' "foo" (D.oa "BAR" (StringValue "qux"))
                    , D.poR "BAR"
                    ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.soptR_ 'x' ['v', 'z', 'f'] ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "file")
                            (Just $ Desc.arg "FILE" (Just "foo"))
                ])
                ([ out [
                    [ D.soptR_ 'x'
                    , D.soptR_ 'v'
                    , D.soptR_ 'z'
                    , D.optR 'f' "file" (D.oa "FILE" (StringValue "foo"))
                    ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.sopt_ 'f' [], U.poR "FILE" ] ] ])
        [ pass  [ Desc.opt (Desc.fname 'f' "file")
                           (Just $ Desc.arg "FILE" (Just "foo"))
                , Desc.opt (Desc.fname 'f' "file")
                           (Just $ Desc.arg "FILE" (Just "foo"))
                ]
                ([ out [
                    [ D.optR 'f' "file" (D.oa "FILE" (StringValue "foo")) ]
                ] ])
        ]

    , test ([ U.usage "foo" [ [ U.soptR_ 'f' ['x'] ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "file")
                            (Just $ Desc.arg "FILE" (Just "foo"))
                ])
                ([ out [
                    [ D.soptR_ 'f'
                    , D.soptR_ 'x'
                    ]
                ] ])
        ]

      -- Note: `f` should not adopt `file` as it's full name since it's in an
      -- option stack and not in trailing position (therefore cannot inherit the
      -- description's argument, rendering it an unfit candidate)
    , test ([ U.usage "foo" [ [ U.soptR_ 'f' ['v', 'z', 'x'] ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "file")
                            (Just $ Desc.arg "FILE" (Just "foo"))
                ])
                ([ out [
                    [ D.soptR_ 'f'
                    , D.soptR_ 'v'
                    , D.soptR_ 'z'
                    , D.soptR_ 'x'
                    ]
                ] ])
        ]
    , test ([ U.usage "foo" [ [ U.soptR_ 'x' ['v', 'z', 'f'] ] ] ])
        [ pass  ([ Desc.opt (Desc.fname 'f' "file")
                            (Just $ Desc.arg "FILE" (Just "foo"))
                ])
                ([ out [
                    [ D.soptR_ 'x'
                    , D.soptR_ 'v'
                    , D.soptR_ 'z'
                    , D.optR 'f' "file" (D.oa "FILE" (StringValue "foo"))
                    ]
                ] ])
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
                (Desc.prettyPrintDesc <$> descs))) do
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
