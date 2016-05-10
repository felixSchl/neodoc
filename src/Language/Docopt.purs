-- |
-- | Docopt surface.
-- |
-- | The pure part of docopt, all functions are invariant.
-- |

module Language.Docopt (
    runDocopt
  , parseDocopt
  , evalDocopt
  ) where

import Prelude ((<<<), bind, return, ($), (<$>))
import Data.Either (Either)
import Data.List (List, concat)
import Data.StrMap (StrMap())
import Data.Tuple (uncurry)
import Data.Bifunctor (lmap)
import Data.Traversable (traverse)
import Text.Parsing.Parser as P
import Text.Wrap (dedent)

import Language.Docopt.Usage (Usage(..)) as D
import Language.Docopt.Errors (Argv, DocoptError(..), SolveError(..),
                              developerErrorMessage, prettyPrintDocoptError
                              ) as D
import Language.Docopt.Value (Value(..)) as D
import Language.Docopt.ParserGen  as G
import Language.Docopt.Trans.Flat as T

import Language.Docopt.Scanner      as Scanner
import Language.Docopt.Solver       as Solver
import Language.Docopt.Parser.Usage as Usage
import Language.Docopt.Parser.Desc  as Desc

type Docopt = {
  usage         :: String
, specification :: List D.Usage
}

data Origin
  = Argv
  | Environment
  | Default

-- |
-- | Parse the docopt text and produce a parser
-- | that can be applied to user input.
-- |
parseDocopt :: String  -- ^ The docopt text
            -> Boolean -- ^ Enable smart-options
            -> Either String Docopt
parseDocopt docopt smartOpts = do
  doc <- toScanErr       $ Scanner.scan $ dedent docopt
  us  <- toUsageParseErr $ Usage.run doc.usage smartOpts
  ds  <- toDescParseErr  $ concat <$> Desc.run `traverse` doc.options
  prg <- toSolveErr      $ Solver.solve us ds
  return $ { specification: prg , usage: doc.usage }

-- |
-- | Apply the generated docopt parser to user input.
-- |
evalDocopt  :: List D.Usage      -- ^ The program specification
            -> StrMap String     -- ^ The environment
            -> Array String      -- ^ The user input
            -> Boolean           -- ^ Enable "options-first"
            -> Either String (StrMap D.Value)
evalDocopt prg env argv optsFirst = do
  vs <- toUserParseErr argv $ G.runParser env argv (G.genParser prg optsFirst)
  return $ uncurry (T.reduce prg env) vs

-- |
-- | Parse the docopt source, derive a parser and then
-- | apply it to user input.
-- |
runDocopt :: String        -- ^ The docopt text
          -> StrMap String -- ^ The environment
          -> Array String  -- ^ The user input
          -> Boolean       -- ^ Enable "options-first"
          -> Boolean       -- ^ Enable "smart-options"
          -> Either String (StrMap D.Value)
runDocopt docopt env argv optsFirst smartOpts = do
  { specification } <- parseDocopt docopt smartOpts
  evalDocopt specification env argv optsFirst

toScanErr :: forall a. Either P.ParseError a -> Either String a
toScanErr  = lmap (D.prettyPrintDocoptError <<< D.DocoptScanError)

toUsageParseErr :: forall a. Either P.ParseError a -> Either String a
toUsageParseErr = lmap (D.prettyPrintDocoptError <<< D.DocoptUsageParseError)

toDescParseErr :: forall a. Either P.ParseError a -> Either String a
toDescParseErr = lmap (D.prettyPrintDocoptError <<< D.DocoptDescParseError)

toUserParseErr :: forall a. Array String -> Either P.ParseError a -> Either String a
toUserParseErr argv = lmap (D.prettyPrintDocoptError <<< D.DocoptUserParseError argv)

toSolveErr :: forall a. Either D.SolveError a -> Either String a
toSolveErr = lmap (D.prettyPrintDocoptError <<< D.DocoptSolveError)
