module Language.Docopt.ParserGen (
    module Language.Docopt.ParserGen.Token
  , genParser
  , runParser
  , Result ()
  , module G
  ) where

import Prelude
import Data.List (List(), toList)
import Data.Either (Either())
import Data.Tuple (Tuple)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State (evalState)
import Text.Parsing.Parser (ParseError, PState(PState), runParserT) as P
import Text.Parsing.Parser.Pos (initialPos) as P

import Language.Docopt.Argument (Branch) as D
import Language.Docopt.Usage (Program) as D
import Language.Docopt.Env (Env) as D
import Language.Docopt.ParserGen.Token (PositionedToken(..), Token(..),
                                        getSource, prettyPrintToken,
                                        unPositionedToken) as G
import Language.Docopt.ParserGen.Parser (Parser, genUsageParser,
                                        initialState, ValueMapping(),
                                        GenOptionsObj()) as G
import Language.Docopt.ParserGen.Lexer (lex) as G

type Result = Tuple D.Branch (List G.ValueMapping)

-- | Generate a parser for a given program specification.
genParser :: forall r
           . D.Program         -- ^ the program to generate a parser for
          -> G.GenOptionsObj r -- ^ Generator opts
          -> G.Parser Result   -- ^ the generated parser
genParser = G.genUsageParser

-- | Run a parser against user input.
runParser :: D.Env                      -- ^ the user input
          -> Array String               -- ^ the environment
          -> G.Parser Result            -- ^ the program parser
          -> Either P.ParseError Result -- ^ the parsed output
runParser env argv p = do
  toks <- G.lex (toList argv)
  evalState (runReaderT (go toks p) env) G.initialState
  where go i = P.runParserT (P.PState { input: i
                                      , position: P.initialPos
                                      })

