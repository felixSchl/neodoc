module Language.Docopt (
  module D
, runDocopt
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

runDocopt :: StrMap String -- ^ The environment
          -> String        -- ^ The docopt text
          -> Array String  -- ^ The user input
          -> Either D.DocoptError (Map String D.Value)
runDocopt env docopt argv = do
  doc <- toScanErr  $ Scanner.scan $ dedent docopt
  us  <- toParseErr $ Usage.run doc.usage
  ds  <- toParseErr $ concat <$> Desc.run `traverse` doc.options
  prg <- toSolveErr $ Solver.solve us (Solver.applyEnvToDesc env ds)
  vs  <- toParseErr $ G.runParser argv (G.genParser prg)
  return $ T.byName $ uncurry T.reduce vs

toScanErr :: forall a. Either P.ParseError a -> Either D.DocoptError a
toScanErr  = lmap D.DocoptScanError

toParseErr :: forall a. Either P.ParseError a -> Either D.DocoptError a
toParseErr = lmap D.DocoptParseError

toSolveErr :: forall a. Either D.SolveError a -> Either D.DocoptError a
toSolveErr = lmap D.DocoptSolveError
