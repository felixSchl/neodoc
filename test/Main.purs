module Test.Main where

import Prelude
import Debug.Trace
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import qualified Text.Parsing.Parser as P
import qualified Docopt.Parser.Docopt as Docopt
import qualified Docopt.Parser.Usage as Usage
import qualified Docopt.Parser.Options as Options
import qualified Docopt.Textwrap as Textwrap
import qualified Docopt.Parser.Lexer as Lexer
import qualified Docopt.Parser.Scanner as Scanner
import Docopt.Parser.Base (debug)
import Data.Either (either)

import Test.Spec (describe, it)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] do
  describe "docopt scanner" do
    it "breaks apart the input" do
      either
        (throwError <<< error <<< show)
        (traceShowA)
        (Scanner.scanDocopt
          """
          Naval Fate.

          Usage:
          naval_fate -xvzf FIle
          """)

  describe "docopt lexer" do
    it "lexes usages" do
      either
        (throwError <<< error <<< show)
        (traceShowA)
        (flip P.runParser Lexer.parseTokens $ Textwrap.dedent
        """ naval_fate -xvzf FIle""")

    it "lexes options" do
      either
        (throwError <<< error <<< show)
        (traceShowA)
        (flip P.runParser Lexer.parseTokens $ Textwrap.dedent
        """
        -h --help     Show this screen.
        --version     Show version.
        --speed=<kn>  Speed in knots [default: 10].
        --moored      Moored (anchored) mine.
        --drifting    Drifting mine.
        """)

  describe "docopt parser" do
    it "parses options" do
      either
        (throwError <<< error <<< show)
        (traceShowA)
        do
          toks <- flip P.runParser Lexer.parseTokens $ Textwrap.dedent
            """
            -h --help     Show this screen.
            --version     Show version.
            --speed=<kn>  Speed in knots [default: 10].
            --moored      Moored (anchored) mine.
            --drifting    Drifting mine.
            """
          flip Lexer.runTokenParser Options.parseOptions toks
