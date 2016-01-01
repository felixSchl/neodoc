-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.
-- |
-- | ===

module Docopt.Spec.Solver where

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
import qualified Docopt.Spec.Parser.Desc  as D
import qualified Docopt.Spec.Parser.Usage as U

data SolveError = SolveError
instance showSolveError :: Show SolveError where
  show _ = "SolveError"

-- | Given a Argument and an Option, unify them into a single Docopt
--   specification Argument.
unify :: U.Argument -> D.Desc -> Argument
unify (U.Option _ _ _) (D.Option _ _ _)
  -- XXX: Actually write this:
  = Option (Just 'f') Nothing Nothing true

solve :: (List U.Usage)
      -> (List D.Desc)
      -> Either SolveError (List Application)
solve us os = do
  Left SolveError
