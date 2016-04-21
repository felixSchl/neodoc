module Language.Docopt.Solver2 where

import Prelude
import Debug.Trace
import Data.Functor
import Data.Function (on)
import Data.Either (Either(..), either)
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..), isJust, maybe, maybe', isNothing)
import Data.List (List(..), filter, head, foldM, concat, (:), singleton
                , catMaybes, toList, last, init, length, groupBy)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (foldl, intercalate)
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Control.Alt ((<|>))
import Control.Apply((*>))
import Data.Monoid (mempty)
import Control.Monad.Error.Class (throwError)
import Data.String (fromCharArray, fromChar, toUpper, charAt, toCharArray)
import Data.Array as A
import Data.String as S
import Data.String.Unsafe as US
import Data.StrMap as StrMap
import Data.StrMap (StrMap())

import Data.String.Ext ((^=), endsWith)
import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Usage
import Language.Docopt.Parser.Desc (Desc(..))
import Language.Docopt.Value (Value(..))
import Language.Docopt.Option                as O
import Language.Docopt.Parser.Desc           as DE
import Language.Docopt.Parser.Usage          as U
import Language.Docopt.Parser.Usage.Argument as U
import Language.Docopt.Parser.Usage.Option   as UO


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
