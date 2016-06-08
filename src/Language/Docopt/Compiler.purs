module Language.Docopt.Compiler (
    run
  , Result ()
  , Options ()
  , module Reexport
  ) where

import Prelude
import Data.List (List(), toList)
import Data.Either (Either())
import Data.Tuple (Tuple(), fst)
import Control.Monad.RWS (RWS(), evalRWS)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State (evalState)
import Text.Parsing.Parser (ParseError, PState(PState), runParserT) as P
import Text.Parsing.Parser.Pos (initialPos) as P

import Language.Docopt.Argument (Branch) as D
import Language.Docopt.Usage (Program) as D
import Language.Docopt.Env (Env) as D
import Language.Docopt.Compiler.Parser (spec, initialState, ValueMapping(),
                                        Options()) as P
import Language.Docopt.Compiler.Lexer (lex, Options()) as L
import Language.Docopt.Compiler.Parser (ValueMapping()) as Reexport


type Result = Tuple D.Branch (List P.ValueMapping)

type Options r = {
  optionsFirst :: Boolean
, stopAt       :: Array String
  | r
}

run
  :: forall r
   . D.Program    -- ^ the user input
  -> D.Env        -- ^ the environment
  -> Array String -- ^ the program parser
  -> Options r
  -> Either P.ParseError Result
run spec env argv options = do
  toks <- L.lex (toList argv) options
  fst $ evalRWS
          (P.runParserT
            (P.PState { input: toks, position: P.initialPos })
            (P.spec spec options)
          )
          env
          P.initialState
