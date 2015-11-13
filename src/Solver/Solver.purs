module Docopt.Solver where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..), filter)

import Docopt (Argument(..), Application(), Branch())
import qualified Docopt.Parser.Options as O
import qualified Docopt.Parser.Usage   as U

solve :: (List U.Usage) -> (List O.Option) -> List Application
solve us os = solveUsage os <$> us

 where
  solveUsage :: (List O.Option) -> U.Usage -> Application
  solveUsage os (U.Usage _ bs) = Application $ solveBranch os <$> bs

  solveBranch :: (List O.Option) -> (List U.UsageNode) -> Branch
  solveBranch os us = solveNode os <$> us

  solveNode :: (List O.Option) -> U.UsageNode -> Argument
  solveNode os (U.Command name)      = Command name
  solveNode os (U.Positional name r) = Positional name r
  solveNode os (U.Group o bs r)      = Group o (solveBranch os <$> bs) r
  solveNode os (U.Option name arg r) =
    let x = filter matching os
     in Command "blah"
       where
         matching = const false


  -- solveNode os (U.OptionStack flag flags arg r)
  --   = unit
