module Test.Spec.DescParserSpec (descParserSpec) where

import Prelude
import Data.Either (Either(..), either)
import Data.Foldable (intercalate, for_)
import Data.Bifunctor (lmap)
import Data.List (toUnfoldable, List(Nil), (:))
import Data.Pretty (pretty)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Maybe (Maybe(..))
import Control.Bind ((=<<))
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.State.Trans (StateT(StateT))
import Text.Wrap (dedent)
import Text.Parsing.Parser as P
import Test.Spec (describe, it)
import Test.Support (vliftEff)
import Test.Support.Desc as Desc

import Neodoc.Value
import Neodoc.Error.Class as Error
import Neodoc.Data.Description
import Neodoc.Data.Description as Description
import Neodoc.Data.OptionArgument
import Neodoc.Spec.Parser.Description as Description
import Neodoc.Spec.Lexer as Lexer
import Neodoc.Spec.Parser as Spec
import Neodoc.OptionAlias
import Neodoc.OptionAlias as OptionAlias

newtype TestCase = TestCase {
  input :: String
, output :: Either String (Array Description)
}

pass :: String -> Array Description -> TestCase
pass input output = TestCase { input: input, output: Right output }

fail :: String -> String -> TestCase
fail input msg = TestCase { input: input, output: Left msg }

o :: {
  aliases    :: NonEmpty List OptionAlias
, arg        :: Maybe OptionArgument
, env        :: Maybe String
, default    :: Maybe Value
, repeatable :: Boolean
} -> Description
o x = OptionDescription x.aliases x.repeatable x.arg x.default x.env

str = StringValue
int = IntValue
arg     n = Desc.argument n false
optarg  n = Desc.argument n true

descParserSpec = \_ ->
  describe "The description parser" do
    for_ [
          pass ("-f enable the --foo flag")
            [ o { aliases:    OptionAlias.Short 'f' :| Nil
                , arg:        Nothing
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                } ]
        , pass ("-f ENABLE the --foo flag")
            [ o { aliases:    OptionAlias.Short 'f' :| Nil
                , arg:        Just $ arg "ENABLE"
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                } ]
        , pass ("-f[=ENABLE] the --foo flag")
            [ o { aliases:    OptionAlias.Short 'f' :| Nil
                , arg:        Just $ optarg "ENABLE"
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                } ]
        , pass ("--foo enable the --foo flag")
            [ o { aliases:    OptionAlias.Long "foo" :| Nil
                , arg:        Nothing
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                } ]
        , pass ("--foo ENABLE the --foo flag")
            [ o { aliases:    OptionAlias.Long "foo" :| Nil
                , arg:        Just $ arg "ENABLE"
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                } ]
        , pass ("--foo[=ENABLE] the --foo flag")
            [ o { aliases:    OptionAlias.Long "foo" :| Nil
                , arg:        Just $ optarg "ENABLE"
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                } ]
        , pass ("-f, --foo...")
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Nothing
                , default:    Nothing
                , env:        Nothing
                , repeatable: true
                } ]
        , pass ("-f=BAZ, --foo=BAZ...")
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Nothing
                , repeatable: true
                }
            ]
          -- XXX: Indecisive here: Should this throw an error instead?
        , pass ("-f=BAZ, --foo[=BAZ]")
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ optarg "BAZ"
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                }
            ]
        , pass ("-f=BAZ, --foo=BAZ [default: 100]")
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Just (int 100)
                , repeatable: false
                }
            ]
        , pass ("-f=BAZ, --foo [default: 100]")
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ, -f [default: 100]")
            [ o { aliases:    OptionAlias.Long "foo" :| OptionAlias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Just (int 100)
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ, -f=BAZ [default: 100]")
            [ o { aliases:    OptionAlias.Long "foo" :| OptionAlias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Just (int 100)
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ  -f=BAZ [default: 100]")
            [ o { aliases:    OptionAlias.Long "foo" :| OptionAlias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Just (int 100)
                , repeatable: false
                }
            ]
        , pass ("--foo  -f=BAZ [default: 100]")
            [ o { aliases:    OptionAlias.Long "foo" :| OptionAlias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Just (int 100)
                , repeatable: false
                }
            ]
        , pass ("--foo=BAZ  -f [default: 100]")
            [ o { aliases:    OptionAlias.Long "foo" :| OptionAlias.Short 'f' :  Nil
                , arg:        Just $ arg "BAZ"
                , env:        Nothing
                , default:    Just (int 100)
                , repeatable: false
                }
            ]

        , pass ("-f, --foo=BAZ [default: 100]")
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
                , env:        Nothing
                , repeatable: false
                }
            ]
        , pass (dedent
            """
            -f=BAZ, --foo=BAZ [default: 100]
            -q=BAZ, --qux=BAZ [default: 200]
            """)
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 200)
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
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 200)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Long "qux" : Nil
                , arg:        Just $ arg "QIZ"
                , default:    Just (int 200)
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
          ) [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 200)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 200)
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
          ) [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 200)
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
          ) [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
                , env:        Nothing
                , repeatable: false
                }
            , o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Long "qux" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 200)
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
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
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
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ"
                , default:    Just (int 100)
                , env:        Just "QARK"
                , repeatable: false
                }
            , o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Long "qux" : Nil
                , arg:        Just $ arg "QIZ"
                , default:    Nothing
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
            [ o { aliases:    OptionAlias.Short 'f' :| OptionAlias.Long "foo" : Nil
                , arg:        Just $ arg "BAZ" -- uses the first occurence
                , env:        Nothing
                , default:    Nothing
                , repeatable: false
                }
            ]

          -- more than two aliases:
        , pass  "-q, -?, --help"
            [ o { aliases:    OptionAlias.Short 'q' :| OptionAlias.Short '?' : OptionAlias.Long "help" : Nil
                , arg:        Nothing
                , env:        Nothing
                , default:    Nothing
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
                  (intercalate "\n" $ pretty <$> out))
                output)) do
        vliftEff $ evaltest (lmap pretty do
            toks <- Error.capture $ Lexer.lexDescs input
            Error.capture $ Spec.parseDescription toks
            ) output

    evaltest (Left msg) (Left msg')
      = if msg == msg'
           then pure unit
           else throwException $ error $ "Unexpected error:\n" <> msg

    evaltest (Left e) _ = throwException $ error $ show e

    evaltest (Right out) (Left _)
      = throwException $ error $
          "Missing exception! Got:\n"
            <> (intercalate "\n" $ pretty <$> out)

    evaltest (Right out) (Right expected)
      = let out' = toUnfoldable out
         in if (out' == expected)
              then pure unit
              else throwException $ error $
                    "Unexpected output:\n"
                      <> (intercalate "\n" $ pretty <$> out')
