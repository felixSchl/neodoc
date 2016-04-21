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
import Data.Tuple (Tuple(..))
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

import Language.Docopt.Usage     as D
import Language.Docopt.Errors    as D
import Language.Docopt.Value     as D
import Language.Docopt.ParserGen as G
import Language.Docopt.Trans     as T

import Language.Docopt.Scanner      as Scanner
import Language.Docopt.Solver       as Solver
import Language.Docopt.Solver2      as Solver2
import Language.Docopt.Parser.Usage as Usage
import Language.Docopt.Parser.Desc  as Desc

type Docopt = {
  usage         :: String
, specification :: List D.Usage
}

-- |
-- | Parse the docopt text and produce a parser
-- | that can be applied to user input.
-- |
parseDocopt :: String  -- ^ The docopt text
            -> Either String Docopt
parseDocopt docopt = do
  doc <- toScanErr       $ Scanner.scan $ dedent docopt
  us  <- toUsageParseErr $ Usage.run doc.usage
  ds  <- toDescParseErr  $ concat <$> Desc.run `traverse` doc.options
  prg <- toSolveErr      $ Solver2.solve us ds
  return $ { specification: prg , usage: doc.usage }

-- |
-- | Apply the generated docopt parser to user input.
-- |
applyDocopt :: List D.Usage      -- ^ The program specification
            -> StrMap String     -- ^ The environment
            -> Array String      -- ^ The user input
            -> Boolean           -- ^ Enable "options-first"
            -> Either String (StrMap D.Value)
applyDocopt prg env argv optsFirst = do
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
          -> Either String (StrMap D.Value)
runDocopt docopt env argv optsFirst = do
  { specification } <- parseDocopt docopt
  applyDocopt specification env argv optsFirst

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
