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
import qualified Docopt.Spec.Parser.Options as O
import qualified Docopt.Spec.Parser.Usage   as U

data SolveError = SolveError
instance showSolveError :: Show SolveError where
  show _ = "SolveError"

data Step
  = Pending (U.UsageNode -> List Argument)
  | Idle    (List Argument)

-- | Solve each usage node by seeing if there are more information to be
-- | gathered from any of the options sections.
-- | This applies only to options, not for positional arguments or commands.
-- | For options, see if the option appears in any option section, and if so,
-- | merge the information into a valid Option construction of the
-- | `Docopt Argument` type.
-- |
-- | Problems to solved conceptually:
-- |    * Should the solver detect discrepancies, i.e. multiple option sections
-- |      talking about the same option in a different way. If compatible,
-- |      should the solver merge them?
-- |    * Can we have command-specific option sections? Such that each usage
-- |      branch may have a different interpretation, i.e. defaults and
-- |      description for a single option?
-- |    * Options should be able to specify their default option in the usage
-- |      section:
-- |
-- |      ```docopt
-- |      Usage: program foo=100
-- |      ```
-- |
-- |      Once implemented, how will the solver treat the case where the
-- |      description specifies another option?
-- |
-- | Other ideas that may need to solved before beginning implementation:
-- |    * Is it time to leave the docopt world and add more things, make the
-- |      language a bit more formal to allow for things like more meta
-- |      information about options. Consider, e.g.:
-- |
-- |      ```docopt
-- |      Usage:
-- |        program <command> --foo=BAR
-- |      Options:
-- |        -f, --foo=BAR lorem ipsum lorem ipsum lorem ipsum lorem
-- |                      ipsum.
-- |                      [Default: 100]
-- |                      [Implies: --qux=200, --verbos]
-- |        <command>     lorem ipsum lorem ipsum
-- |                      [Choices: qux|foo|bar]
-- |      ```
-- |

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
