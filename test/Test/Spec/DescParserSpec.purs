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

import Language.Docopt
import qualified Language.Docopt.Parser.Desc as Desc
import qualified Language.Docopt.Parser.Lexer as Lexer
import Language.Docopt.Parser.Base (debug)
import Text.Wrap (dedent)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Test.Support (vliftEff)

newtype TestCase = TestCase { input :: String
                            , output :: Either String (Array Desc.Desc) }

pass :: String -> Array Desc.Desc -> TestCase
pass input output = TestCase { input: input, output: Right output }

fail :: String -> String -> TestCase
fail input msg = TestCase { input: input, output: Left msg }

o = Desc.OptionDesc <<< Desc.Option

str = StringValue
int = IntValue
arg_    n   = Desc.argument n false Nothing
arg     n v = Desc.argument n false (Just v)
optarg  n v = Desc.argument n true (Just v)
optarg_ n   = Desc.argument n true Nothing

descParserSpec = \_ ->
  describe "The description parser" do
    for_ [
          pass ("-f enable the --foo flag")
            [ o { name: Desc.Flag 'f'
                , arg: Nothing
                , env: Nothing } ]
        , pass ("-f ENABLE the --foo flag")
            [ o { name: Desc.Flag 'f'
                , arg: Just $ arg_ "ENABLE"
                , env: Nothing } ]
        , pass ("-f[=ENABLE] the --foo flag")
            [ o { name: Desc.Flag 'f'
                , arg: Just $ optarg_ "ENABLE"
                , env: Nothing } ]
        , pass ("-f [=ENABLE] the --foo flag")
            [ o { name: Desc.Flag 'f'
                , arg: Just $ optarg_ "ENABLE"
                , env: Nothing } ]
        , pass ("--foo enable the --foo flag")
            [ o { name: Desc.Long "foo"
                , arg: Nothing
                , env: Nothing
                } ]
        , pass ("--foo ENABLE the --foo flag")
            [ o { name: Desc.Long "foo"
                , arg: Just $ arg_ "ENABLE"
                , env: Nothing
                } ]
        , pass ("--foo[=ENABLE] the --foo flag")
            [ o { name: Desc.Long "foo"
                , arg: Just $ optarg_ "ENABLE"
                , env: Nothing
                } ]
        , pass ("--foo [=ENABLE] the --foo flag")
            [ o { name: Desc.Long "foo"
                , arg: Just $ optarg_ "ENABLE"
                , env: Nothing
                } ]
        , pass ("-f, --foo")
            [ o { name: Desc.Full 'f' "foo"
                , arg: Nothing
                , env: Nothing
                } ]
        , pass ("-f=BAZ, --foo=BAZ")
            [ o { name: Desc.Full 'f' "foo"
                , arg:  Just $ arg_ "BAZ"
                , env:  Nothing }
            ]
        , pass ("-f=BAZ, --foo=BAZ [default: 100]")
            [ o { name: Desc.Full 'f' "foo"
                , arg:  Just $ arg "BAZ" (int 100)
                , env:  Nothing
                }
            ]
        , pass (dedent
            """
            -f=BAZ, --foo=BAZ [default: 100]
            -q=BAZ, --qux=BAZ [default: 200]
            """)
            [ o { name: Desc.Full 'f' "foo"
                , arg:  Just $ arg "BAZ" (int 100)
                , env:  Nothing
                }
            , o { name: Desc.Full 'q' "qux"
                , arg:  Just $ arg "BAZ" (int 200)
                , env:  Nothing
                }
            ]
        , pass (dedent
            """
            -f=BAZ, --foo=BAZ this is
                              some more text
                              [default: 100]

            -q=QIZ, --qux=QIZ this is also more
              text [default: 200]
            """)
            [ o { name: Desc.Full 'f' "foo"
                , arg:  Just $ arg "BAZ" (int 100)
                , env:  Nothing
                }
            , o { name: Desc.Full 'q' "qux"
                , arg:  Just $ arg "QIZ" (int 200)
                , env:  Nothing
                }
            ]
        , fail
              "-f=BAZ, --foo=qux"
              "Arguments mismatch: \"BAZ\" and \"qux\""
        , pass (dedent
            """
            -f=BAZ, --foo=BAZ this is some more text [default: 100]
                                                     [env: QARK]
                -q=QIZ, --qux=QIZ this option is over-indented and won't
                                  be parsed.
            """)
            [ o { name: Desc.Full 'f' "foo"
                , arg:  Just $ arg "BAZ" (int 100)
                , env:  Just "QARK"
                }
            ]
        , fail (dedent
            """
            -q, --qux
            -f, --foo this option takes no arg, `default`
                      is invalid [default: 100]
            """)
            "Option \"-f, --foo\" does not take arguments. Cannot specify defaults."
        , fail (dedent
            """
            -f=BAZ, --foo=BAZ [default: 100] [default: 100]
            """)
            "Option \"-f, --foo=BAZ\" has multiple defaults!"
        , fail (dedent
            """
            -f=BAZ, --foo=BAZ [env: FOO_BAR] [env: BAR_FOO]
            """)
            "Option \"-f, --foo=BAZ\" has multiple environment mappings!"
        ]
        runtest
  where
    runtest (TestCase { input, output }) = do
      it (input ++ " " ++
        (either (\msg -> "should fail with \"" ++ msg ++ "\"")
                (\out -> "should succeed with:\n" ++
                  (intercalate "\n" $ Desc.prettyPrintDesc <$> out))
                output)) do
        vliftEff $ evaltest (Desc.parse =<< Lexer.lexDescs input) output

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
