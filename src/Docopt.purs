module Docopt (
  module Docopt.Types
, module Docopt.Pretty
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
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Apply ((*>))
import Data.Bifunctor (lmap)
import Data.Traversable (traverse)
import Docopt.Types
import Docopt.Pretty
import qualified Docopt.Gen as Gen
import qualified Docopt.Spec.Parser.Scanner as Scanner
import qualified Docopt.Spec.Parser.Usage as Usage
import qualified Docopt.Spec.Parser.Desc as Desc
import qualified Docopt.Spec.Solver as Solver
import qualified Text.Parsing.Parser as P
import Text.Wrap (dedent)

runDocopt :: String
          -> Array String
          -> Either DocoptError (Map Argument Value)
runDocopt docopt argv = do
  docopt <- toScanErr  $ Scanner.scan $ dedent docopt
  us     <- toParseErr $ Usage.run docopt.usage
  ds     <- toParseErr $ concat <$> Desc.run `traverse` docopt.options
  solved <- toSolveErr $ Solver.solve us ds
  toParseErr $ Gen.run solved (toList argv)

toScanErr :: forall a. Either P.ParseError a -> Either DocoptError a
toScanErr  = lmap DocoptScanError

toParseErr :: forall a. Either P.ParseError a -> Either DocoptError a
toParseErr = lmap DocoptParseError

toSolveErr :: forall a. Either SolveError a -> Either DocoptError a
toSolveErr = lmap DocoptSolveError
