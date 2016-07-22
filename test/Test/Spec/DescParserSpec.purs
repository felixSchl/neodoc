module Test.Spec.DescParserSpec (descParserSpec) where

import Prelude
import Language.Docopt.SpecParser.Desc as Desc
import Language.Docopt.SpecParser.Lexer as Lexer
import Text.Parsing.Parser as P
import Control.Bind ((=<<))
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.State.Trans (StateT(StateT))
import Data.Either (Either(..), either)
import Data.Foldable (intercalate, for_)
import Data.List (toUnfoldable, List(Nil), (:))
import Data.Maybe (Maybe(..))
import Language.Docopt.Value (Value(..))
import Language.Docopt.OptionAlias ((:|))
import Language.Docopt.OptionAlias (OptionAlias(..)) as Alias
import Test.Spec (describe, it)
import Test.Support (vliftEff)
import Text.Wrap (dedent)
import Test.Support.Desc as Desc

newtype TestCase = TestCase { input :: String
                            , output :: Either String (Array Desc.Desc) }

pass :: String -> Array Desc.Desc -> TestCase
pass input output = TestCase { input: input, output: Right output }

fail :: String -> String -> TestCase
fail input msg = TestCase { input: input, output: Left msg }

o = Desc.OptionDesc

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
            [ o { aliases:    Alias.Short 'f' :| Nil
                , arg:        Nothing
                , env:        Nothing
                , repeatable: false
                } ]
        , pass ("-f ENABLE the --foo flag")
            [ o { aliases:    Alias.Short 'f' :| Nil
                , arg:        Just $ arg_ "ENABLE"
                , env:        Nothing
                , repeatable: false
                } ]
        , pass ("-f[=ENABLE] the --foo flag")
            [ o { aliases:    Alias.Short 'f' :| Nil
                , arg:        Just $ optarg_ "ENABLE"
                , env:        Nothing
                , repeatable: false
                } ]
        , pass ("--foo enable the --foo flag")
            [ o { aliases:    Alias.Long "foo" :| Nil
                , arg:        Nothing
                , env:        Nothing
                , repeatable: false
                } ]
        , pass ("--foo ENABLE the --foo flag")
            [ o { aliases:    Alias.Long "foo" :| Nil
                , arg:        Just $ arg_ "ENABLE"
                , env:        Nothing
                , repeatable: false
                } ]
        , pass ("--foo[=ENABLE] the --foo flag")
            [ o { aliases:    Alias.Long "foo" :| Nil
                , arg:        Just $ optarg_ "ENABLE"
                , env:        Nothing
                , repeatable: false
                } ]
        , pass ("-f, --foo...")
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Nothing
                , env:        Nothing
                , repeatable: true
                } ]
        , pass ("-f=BAZ, --foo=BAZ...")
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg_ "BAZ"
                , env:        Nothing
                , repeatable: true
                }
            ]
          -- XXX: Indecisive here: Should this throw an error instead?
        , pass ("-f=BAZ, --foo[=BAZ]")
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ optarg_ "BAZ"
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("-f=BAZ, --foo=BAZ [default: 100]")
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("-f=BAZ, --foo [default: 100]")
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ, -f [default: 100]")
            [ o { aliases:    Alias.Long "foo" :| Alias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ, -f=BAZ [default: 100]")
            [ o { aliases:    Alias.Long "foo" :| Alias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ  -f=BAZ [default: 100]")
            [ o { aliases:    Alias.Long "foo" :| Alias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("--foo  -f=BAZ [default: 100]")
            [ o { aliases:    Alias.Long "foo" :| Alias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ  -f [default: 100]")
            [ o { aliases:    Alias.Long "foo" :| Alias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]

        , pass ("-f, --foo=BAZ [default: 100]")
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass (dedent
            """
            -f=BAZ, --foo=BAZ [default: 100]
            -q=BAZ, --qux=BAZ [default: 200]
            """)
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    Alias.Short 'q' :| Alias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ" (int 200)
                , env:        Nothing
                , repeatable: false
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
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    Alias.Short 'q' :| Alias.Long "qux" : Nil
                , arg:        Just $ arg "QIZ" (int 200)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , fail
              "-f=BAZ, --foo=qux"
              "Option-arguments mismatch: \"BAZ\" and \"qux\""
        , pass (dedent
          -- Fix #41
          -- Should allow non-empty option sections that contain no options.
          """
          foo bar
          """
          ) []
        , pass (dedent
          -- Fix #41
          -- Should allow non-empty option sections that contain no options.
          """
          foo bar
          -f=BAZ, --foo=BAZ [default: 100]
          -q=BAZ, --qux=BAZ [default: 200]
          """
          ) [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    Alias.Short 'q' :| Alias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ" (int 200)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass (dedent
          -- Fix #41
          -- Should allow non-empty option sections that contain no options.
          """
          -f=BAZ, --foo=BAZ [default: 100]
          foo bar
          -q=BAZ, --qux=BAZ [default: 200]
          """
          ) [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    Alias.Short 'q' :| Alias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ" (int 200)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass (dedent
          -- Fix #41
          -- Should allow non-empty option sections that contain no options.
          """
          foo bar
          -f=BAZ, --foo=BAZ [default: 100]
          foo bar
          -q=BAZ, --qux=BAZ [default: 200]
          foo bar
          """
          ) [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    Alias.Short 'q' :| Alias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ" (int 200)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass (dedent
            -- if an option is indented past the start of the description
            -- block for the previous option, it's considered part of the
            -- description.
            """
            -f=BAZ, --foo=BAZ this is some more text [default: 100]
                                                     [env: QARK]
                                -q=QIZ, --qux=QIZ this option is over-indented and won't
                                                  be parsed.
            """)
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Just "QARK"
                , repeatable: false
                }
            ]
        , pass (dedent
            """
            -f=BAZ, --foo=BAZ this is some more text [default: 100]
                [env: QARK]
                          -q=QIZ, --qux=QIZ this option is over-indented and won't
                                            be parsed.
            """)
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" (int 100)
                , env:        Just "QARK"
                , repeatable: false
                }
            , o { aliases:    Alias.Short 'q' :| Alias.Long "qux" : Nil
                , arg:        Just $ arg_ "QIZ"
                , env:        Nothing
                , repeatable: false
                }
            ]
        , fail (dedent
            """
            -q, --qux
            -f, --foo this option takes no arg, `default`
                      is invalid [default: 100]
            """)
            "Option -f, --foo does not take arguments. Cannot specify defaults."
        , fail (dedent
            """
            -f=BAZ, --foo=BAZ [default: 100] [default: 100]
            """)
            "Option -f, --foo has multiple defaults!"
        , fail (dedent
            """
            -f=BAZ, --foo=BAZ [env: FOO_BAR] [env: BAR_FOO]
            """)
            "Option -f, --foo has multiple environment mappings!"

        , fail  "-f=BAZ, --foo=BAX"
            "Option-arguments mismatch: \"BAZ\" and \"BAX\""

        , pass  "-f=BAZ, --foo=<baz>" -- BAZ be considered equal to <baz>
            [ o { aliases:    Alias.Short 'f' :| Alias.Long "foo" : Nil
                , arg:        Just $ arg_ "BAZ" -- uses the first occurence
                , env:        Nothing
                , repeatable: false
                }
            ]

          -- more than two aliases:
        , pass  "-q, -?, --help"
            [ o { aliases:    Alias.Short 'q' :| Alias.Short '?' : Alias.Long "help" : Nil
                , arg:        Nothing
                , env:        Nothing
                , repeatable: false
                }
            ]
        ]
        runtest
  where
    runtest (TestCase { input, output }) = do
      it (input <> " " <>
        (either (\msg -> "should fail with \"" <> msg <> "\"")
                (\out -> "should succeed with:\n" <>
                  (intercalate "\n" $ Desc.prettyPrintDesc <$> out))
                output)) do
        vliftEff $ evaltest (Desc.parse =<< Lexer.lexDescs input) output

    evaltest (Left (P.ParseError msg _ _)) (Left msg')
      = if msg == msg'
           then pure unit
           else throwException $ error $ "Unexpected error:\n" <> msg

    evaltest (Left e) _ = throwException $ error $ show e

    evaltest (Right out) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n"
            <> (intercalate "\n" $ Desc.prettyPrintDesc <$> out)

    evaltest (Right out) (Right expected)
      = let out' = toUnfoldable out
         in if (out' == expected)
              then pure unit
              else throwException $ error $
                    "Unexpected output:\n"
                      <> (intercalate "\n" $ Desc.prettyPrintDesc <$> out')
