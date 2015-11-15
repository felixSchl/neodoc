module Docopt.Solver where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.List (List(..), filter, head, foldM)
import Data.Traversable (traverse)
import Data.Foldable (foldl)
import Control.Plus (empty)
import Data.Monoid (mempty)

import Docopt (Argument(..), Application(..), Branch(..))
import qualified Docopt.Parser.Options as O
import qualified Docopt.Parser.Usage   as U

data SolveError = SolveError
instance showSolveError :: Show SolveError where
  show _ = "SolveError"

data Step
  = Pending (U.UsageNode -> List Argument)
  | Idle    (List Argument)

solve :: (List U.Usage)
      -> (List O.Option)
      -> Either SolveError (List Application)
solve us os = traverse solveUsecase us

 where
  solveUsecase :: U.Usage -> Either SolveError Application
  solveUsecase (U.Usage _ bs) = Application <$> do
    traverse solveBranch bs

  solveBranch :: (List U.UsageNode) -> Either SolveError Branch
  solveBranch ns = Branch <$> do
    let x = foldl step (Idle empty) ns
    return empty

    where
      step :: Step -> U.UsageNode -> Step
      step acc u = Idle empty
