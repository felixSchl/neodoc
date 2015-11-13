module Docopt.Solver where

import Prelude
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.List (List(..), filter, head)

import Docopt (Argument(..), Application(..), Branch(..))
import qualified Docopt.Parser.Options as O
import qualified Docopt.Parser.Usage   as U

solve :: (List U.Usage) -> (List O.Option) -> List Application
solve us os = solveUsage os <$> us

 where
  solveUsage :: (List O.Option) -> U.Usage -> Application
  solveUsage os (U.Usage _ bs) = Application $ solveBranch os <$> bs

  solveBranch :: (List O.Option) -> (List U.UsageNode) -> Branch
  solveBranch os us = Branch $ solveNode os <$> us

  solveNode :: (List O.Option) -> U.UsageNode -> Argument
  solveNode os (U.Command name)      = Command name
  solveNode os (U.Positional name r) = Positional name r
  solveNode os (U.Group o bs r)      = Group o (solveBranch os <$> bs) r
  solveNode os (U.Option name arg r) =
    case (head $ filter f os) of
      Nothing ->
        Option Nothing (Just name) arg Nothing r
      Just (O.Option short long default) ->
        Option
          (maybe Nothing (\(O.ShortOption f _) -> Just f) short)
          (maybe Nothing (\(O.LongOption  n _) -> Just n) long)
          arg default r
      where
        f (O.Option _ (Just (O.LongOption n a)) _) = (n == name) && (a == arg)
        f _                                        = false
  -- solveNode os (U.OptionStack f fs arg r) =
