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
import qualified Data.Map as Map
import Control.Apply ((*>))
import Data.Bifunctor (lmap)
import Data.Traversable (traverse)
import qualified Text.Parsing.Parser as P
import Text.Wrap (dedent)

import qualified Language.Docopt.Types           as D
import qualified Language.Docopt.Value           as D
import qualified Language.Docopt.Pretty          as D
import qualified Language.Docopt.ParserGen       as G
import qualified Language.Docopt.ParserGen.Trans as T

import qualified Language.Docopt.Scanner      as Scanner
import qualified Language.Docopt.Solver       as Solver
import qualified Language.Docopt.Parser.Usage as Usage
import qualified Language.Docopt.Parser.Desc  as Desc

runDocopt :: String -- ^ The docopt text
          -> Array String -- ^ The user input
          -> Either D.DocoptError (Map String D.Value)
runDocopt docopt argv = do
  docopt <- toScanErr  $ Scanner.scan $ dedent docopt
  us     <- toParseErr $ Usage.run docopt.usage
  ds     <- toParseErr $ concat <$> Desc.run `traverse` docopt.options
  solved <- toSolveErr $ Solver.solve us ds
  xs     <- toParseErr $ G.runParser (toList argv) (G.genParser solved)
  return (T.byName xs)

toScanErr :: forall a. Either P.ParseError a -> Either D.DocoptError a
toScanErr  = lmap D.DocoptScanError

toParseErr :: forall a. Either P.ParseError a -> Either D.DocoptError a
toParseErr = lmap D.DocoptParseError

toSolveErr :: forall a. Either D.SolveError a -> Either D.DocoptError a
toSolveErr = lmap D.DocoptSolveError
