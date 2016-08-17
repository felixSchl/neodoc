{-
# Simplify the spec by unnesting groups.

* Singleton groups that contain only a single other group, or "required" groups
  that contain only a single required element. Since the set of "required"
  arguments changes with the 'options.requireFlags' setting, only consider
  positional arguments and commands truely required... As a special case, we
  consider a group compromised entirely of "free" arguments (i.e. options and
  flags) to be "similar enough" to the list of singleton groups, where each
  singleton group is compromised of one component of the original listing.

  Consider:
    * (a)       = a
    * ((a))     = (a) -> a
    * ((a b c)) = (a b c)
    * ([a])     = [a]
    * [([a])]   = [a]
    * [-abc]    = [-a] [-b] [-c]                                  (special case)
    * [-abc -e] = [-a] [-b] [-c] [-e]                             (special case)
    * [-abc -e] = [-a] [-b] [-c] [-e]                             (special case)
    * [-abc  x] = [-abc x]                   (note: no expansion because of 'x')
    * (a...)    = a...
    * (a)...    = a...
    * ((a)...)  = (a)... = a...
    * ((a... b c)...) = ((a... b c)...)    (note: no expansion because of '...')
    * ([a])     = [a]
    * [([a])]   = [a]

  This essentially canonicalises the specification. No meaning is lost in the
  process. At least not in terms of parsing user input against the specification
  later on.
-}

module Language.Docopt.Solver.Ungroup where

import Prelude
import Data.List (List(), singleton)
import Language.Docopt

unGroup :: Specification -> Specification
unGroup spec = do
  usage <- spec
  pure do
    branch <- usage
    pure do
      arg <- branch
      unGroupArg arg

unGroupArg :: Argument -> List Argument
-- unGroupArg (Group { branches }) = 
unGroupArg a = singleton a
