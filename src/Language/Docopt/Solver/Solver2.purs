module Language.Docopt.Solver2 where

import Prelude
import Debug.Trace (traceShow)
import Data.Function (on)
import Data.Either (Either)
import Data.List (List(Nil), groupBy)
import Data.Traversable (traverse)

import Language.Docopt.Errors (SolveError)
import Language.Docopt.Argument (Argument, Branch(Branch))
import Language.Docopt.Usage (Usage(Usage))
import Language.Docopt.Parser.Desc (Desc())
import Language.Docopt.Parser.Usage (Usage(Usage)) as U
import Language.Docopt.Parser.Usage.Argument (Argument, Branch, isFree) as U

solve :: List U.Usage
      -> List Desc
      -> Either SolveError (List Usage)
solve us ds = traverse (flip solveUsage ds) us

solveUsage :: U.Usage -> List Desc -> Either SolveError Usage
solveUsage (U.Usage _ bs) ds = Usage <$> do traverse (flip solveBranch ds) bs

solveBranch :: U.Branch                 -- ^ the usage branch
            -> List Desc                -- ^ the option descriptions
            -> Either SolveError Branch -- ^ the canonical usage branch
solveBranch args descs = Branch <$> go args descs
  where
    go :: List U.Argument
       -> List Desc
       -> Either SolveError (List Argument)
    go as _ =
      let
        -- First step, group adjacent free and fixed arguments.
        -- This leaves us with e.g. '[ [ free, free ], [ fixed ], [ free ] ]'
        gs = groupBy (eq `on` U.isFree) as

        -- Second step, check at the boundaries

       in traceShow gs \_-> return Nil
