module Language.Docopt.ParserGen (
    module Language.Docopt.ParserGen.Token
  , genParser
  , runParser
  , Result ()
  , module G
  ) where

import Prelude
import Debug.Trace
import Data.List (List(..))
import Data.Either (Either(..))
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Map as Map
import Data.Map (Map())
import Data.List (toList)
import Data.Tuple (Tuple(..), uncurry)
import Data.Foldable (foldl)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.Reader (Reader(), ask, runReader)
import Control.Monad.Reader.Trans (ReaderT(), runReaderT)
import Control.Monad.State (State(), runState, evalState, execState)
import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.Combinators as P

import Language.Docopt.Value    as D
import Language.Docopt.Errors   as D
import Language.Docopt.Argument as D
import Language.Docopt.Usage    as D
import Language.Docopt.Env      as D
import Language.Docopt.ParserGen.Token        as G
import Language.Docopt.ParserGen.Parser       as G
import Language.Docopt.ParserGen.Lexer        as G
import Language.Docopt.ParserGen.ValueMapping as G

type Result = Tuple D.Branch (List G.ValueMapping)

-- | Generate a parser for a given program specification.
genParser :: D.Program       -- ^ the program to generate a parser for
          -> Boolean         -- ^ Enable "options-first"
          -> G.Parser Result -- ^ the generated parser
genParser us optsFirst = G.genUsageParser us optsFirst

-- | Run a parser against user input.
runParser :: D.Env                      -- ^ the user input
          -> Array String               -- ^ the environment
          -> G.Parser Result            -- ^ the program parser
          -> Either P.ParseError Result -- ^ the parsed output
runParser env argv p = do
  toks <- G.lex (toList argv)
  evalState (runReaderT (runParser toks p) env) 0
  where runParser i = P.runParserT (P.PState { input: i
                                             , position: P.initialPos
                                             })

