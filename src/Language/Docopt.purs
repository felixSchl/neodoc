-- |
-- | Docopt surface.
-- |
-- | The pure part of docopt, all functions are invariant.
-- |

module Language.Docopt (
    runDocopt
  , parseDocopt
  , applyDocopt
  , module D
  ) where

import Prelude
import Debug.Trace
import Data.Either (either, Either(..))
import Data.Maybe (maybe, Maybe(..))
import Data.List (toList, List(..), concat)
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar)
import Data.Map (Map())
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Tuple (uncurry)
import Data.Map as Map
import Control.Apply ((*>))
import Data.Bifunctor (lmap)
import Data.Traversable (traverse)
import Text.Parsing.Parser as P
import Text.Wrap (dedent)

import Language.Docopt.Errors    as D
import Language.Docopt.Value     as D
import Language.Docopt.ParserGen as G
import Language.Docopt.Trans     as T

import Language.Docopt.Scanner      as Scanner
import Language.Docopt.Solver       as Solver
import Language.Docopt.Parser.Usage as Usage
import Language.Docopt.Parser.Desc  as Desc

-- |
-- | Parse the docopt text and produce a parser
-- | that can be applied to user input.
-- |
parseDocopt :: String  -- ^ The docopt text
            -> Either D.DocoptError (G.Parser G.Result)
parseDocopt docopt = do
  doc <- toScanErr  $ Scanner.scan $ dedent docopt
  us  <- toParseErr $ Usage.run doc.usage
  ds  <- toParseErr $ concat <$> Desc.run `traverse` doc.options
  prg <- toSolveErr $ Solver.solve us ds
  return (G.genParser prg)

-- |
-- | Apply the generated docopt parser to user input.
-- |
applyDocopt :: G.Parser G.Result -- ^ the generated parser
            -> StrMap String     -- ^ the environment
            -> Array String      -- ^ ARGV
            -> Either D.DocoptError (Map String D.Value)
applyDocopt p env argv = do
  vs <- toParseErr $ G.runParser env argv p
  return $ T.byName $ uncurry (T.reduce env) vs

-- |
-- | Parse the docopt source, derive a parser and then
-- | apply it to user input.
-- |
runDocopt :: String        -- ^ The docopt text
          -> StrMap String -- ^ The environment
          -> Array String  -- ^ ARGV
          -> Either D.DocoptError (Map String D.Value)
runDocopt docopt env argv = do
  p <- parseDocopt docopt
  applyDocopt p env argv

toScanErr :: forall a. Either P.ParseError a -> Either D.DocoptError a
toScanErr  = lmap D.DocoptScanError

toParseErr :: forall a. Either P.ParseError a -> Either D.DocoptError a
toParseErr = lmap D.DocoptParseError

toSolveErr :: forall a. Either D.SolveError a -> Either D.DocoptError a
toSolveErr = lmap D.DocoptSolveError
