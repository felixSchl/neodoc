module Test.Spec.DescParserSpec (descParserSpec) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Debug.Trace
import Data.List (fromList)
import Data.Either (Either(..), either)
import Control.Bind ((=<<))
import Data.Maybe (Maybe(..))
import Data.Foldable (intercalate, for_)
import Control.Monad.Eff.Exception (error, throwException)
import qualified Text.Parsing.Parser as P

import Docopt
import qualified Docopt.Spec.Parser.Desc as Desc
import qualified Docopt.Spec.Parser.Lexer as Lexer
import Docopt.Spec.Parser.Base (debug)
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff, runMaybeEff, runEitherEff)

newtype TestCase = TestCase { input :: String
                            , output :: Either String (Array Desc.Desc) }

pass :: String -> Array Desc.Desc -> TestCase
pass input output = TestCase { input: input, output: Right output }

fail :: String -> String -> TestCase
fail input msg = TestCase { input: input, output: Left msg }

o = Desc.OptionDesc

descParserSpec =
  describe "description parser" do
    for_ [
          pass (dedent
            """
            -f enable the --foo flag
            """)
            [ o { flag:    Just 'f'
                , long:    Nothing
                , arg:     Nothing
                , default: Nothing }
            ]
        , pass (dedent
            """
            --foo enable the --foo flag
            """)
            [ o { flag:    Nothing
                , long:    Just "foo"
                , arg:     Nothing
                , default: Nothing }
            ]
        , pass (dedent
            """
            -f, --foo
            """)
            [ o { flag:    Just 'f'
                , long:    Just "foo"
                , arg:     Nothing
                , default: Nothing }
            ]
        , pass (dedent
            """
            -f=baz, --foo=baz
            """)
            [ o { flag:    Just 'f'
                , long:    Just "foo"
                , arg:     Just "baz"
                , default: Nothing }
            ]
        , pass (dedent
            """
            -f=baz, --foo=baz [default: 100]
            """)
            [ o { flag:    Just 'f'
                , long:    Just "foo"
                , arg:     Just "baz"
                , default: Just "100" }
            ]
        , pass (dedent
            """
            -f=baz, --foo=baz [default: 100]
            -q=baz, --qux=baz [default: 100]
            """)
            [ o { flag:    Just 'f'
                , long:    Just "foo"
                , arg:     Just "baz"
                , default: Just "100" }
            , o { flag:    Just 'q'
                , long:    Just "qux"
                , arg:     Just "baz"
                , default: Just "100" }
            ]
        , fail -- XXX: Make this actually fail!
              "-f=baz, --foo=qux"
              "Arguments mismatch: \"baz\" \"qux\""
        ]
        runtest
  where
    runtest (TestCase { input=input, output=output }) = do
      it (input ++ " " ++
        (either (\msg -> "should fail with \"" ++ msg ++ "\"")
                (\out -> "should succeed with:\n" ++
                  (intercalate "\n" $ Desc.prettyPrintDesc <$> out))
                output)) do
        vliftEff $ evaltest (Desc.parse =<< Lexer.lex input) output

    evaltest (Left (P.ParseError { message: msg })) (Left msg')
      = if msg == msg'
           then return unit
           else throwException $ error $ "Unexpected error:\n" ++ msg

    evaltest (Left e) _ = throwException $ error $ show e

    evaltest (Right out) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n"
            ++ (intercalate "\n" $ Desc.prettyPrintDesc <$> out)

    evaltest (Right out) (Right expected)
      = let out' = fromList out
         in if (out' == expected)
              then return unit
              else throwException $ error $
                    "Unexpected output:\n"
                      ++ (intercalate "\n" $ Desc.prettyPrintDesc <$> out')
