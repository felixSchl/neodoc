module Language.Docopt.ParserGen (
    module Language.Docopt.ParserGen.Token
  , genParser
  , runParser
  , Result ()
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
import Control.Monad.Reader (runReader)
import Text.Parsing.Parser     as P
import Text.Parsing.Parser.Pos as P

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
          -> G.Parser Result -- ^ the generated parser
genParser us = foldl (<|>) empty (G.genUsageParser <$> us)

-- | Run a parser against user input.
runParser :: D.Env                      -- ^ the user input
          -> Array String               -- ^ the environment
          -> G.Parser Result            -- ^ the program parser
          -> Either P.ParseError Result -- ^ the parsed output
runParser env argv p = do
  toks <- G.lex (toList argv)
  runReader (runParser toks p) env
  where runParser i = P.runParserT (P.PState { input: i
                                             , position: P.initialPos
                                             })

