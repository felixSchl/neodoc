-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.
-- |
-- | ===

module Docopt.Spec.Solver where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.List (List(..), filter, head, foldM, concat, (:), singleton)
import Data.Traversable (traverse)
import Data.Foldable (foldl)
import Control.Plus (empty)
import Data.Monoid (mempty)

import Docopt (Argument(..), Application(..), Branch(..))
import qualified Docopt.Spec.Parser.Desc  as D
import qualified Docopt.Spec.Parser.Usage as U

data SolveError = SolveError
instance showSolveError :: Show SolveError where
  show _ = "SolveError"

findOptionDesc :: List D.Desc -> U.Argument -> Maybe D.Desc
findOptionDesc _ (U.Command _)      = Nothing
findOptionDesc _ (U.Positional _ _) = Nothing
findOptionDesc _ (U.Option n _ _)   = head $ filter matches ds
  where matches _ = false
findOptionDesc ds arg = head $ filter matches ds
  where matches _ = false

solveArg :: U.Argument -> List D.Desc -> Either SolveError Argument
solveArg (U.Command s) _       = return (Command s)
solveArg (U.Positional s r) _  = return (Positional s r)
solveArg o@(U.Option s a r) ds = do
  let ref = findOptionDesc ds o
  Left SolveError
solveArg (U.Group o bs r) ds  = flip (Group o) r <$> do
  foldM go empty bs
    where go :: List Branch -> U.Branch -> Either SolveError (List Branch)
          go a b = do
            br <- solveBranch b ds
            return (br:a)

solveBranch :: U.Branch -> List D.Desc -> Either SolveError Branch
solveBranch b ds = Left SolveError

solveUsage :: U.Usage -> List D.Desc -> Either SolveError Application
solveUsage (U.Usage _ bs) ds = Application <$> (foldM step Nil bs)
  where
    step :: List Branch -> U.Branch -> Either SolveError (List Branch)
    step bs b = do
      x <- solveBranch b ds
      return $ bs ++ (singleton x)

solve :: (List U.Usage)
      -> (List D.Desc)
      -> Either SolveError (List Application)
solve us ds = foldM step Nil us
  where
    step :: List Application -> U.Usage -> Either SolveError (List Application)
    step as u = do
      x <- solveUsage u ds
      return $ as ++ (singleton x)
