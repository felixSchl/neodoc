{-
# Simplify the spec by unnesting groups.

Consider:

* (a)       = a
* ((a))     = (a) = a
* ((a b c)) = (a b c)
* ([a])     = [a]
* [([a])]   = [a]
* [-abc]    = [-a] [-b] [-c]                                      (special case)
* [-abc -e] = [-a] [-b] [-c] [-e]                                 (special case)
* [-abc -e] = [-a] [-b] [-c] [-e]                                 (special case)
* [-abc  x] = [-abc x]                       (note: no expansion because of 'x')
* (a...)    = a...
* (a)...    = a...
* ((a)...)  = (a)... = a...
* ((a... b c)...) = ((a... b c)...)        (note: no expansion because of '...')
* ([a])     = [a]
* [([a])]   = [a]

This essentially canonicalises the specification. No meaning is lost in the
process. At least not in terms of parsing user input against the specification
later on.
-}

module Language.Docopt.Solver.Canonicalise where

import Prelude
import Data.List (List(Nil), (:), singleton)
import Language.Docopt.Specification
import Language.Docopt.Argument

canonicalise :: Specification -> Specification
canonicalise = (((canonicaliseArg <$> _) <$> _) <$> _)

canonicaliseArg :: Argument -> Argument

-- (a) => a
canonicaliseArg (Group {
    optional: false
  , branches: (x : Nil) : Nil
  , repeatable
  }) |  isPositional x ||
        isCommand    x ||
        (isOption    x &&
          not isFlag x &&  -- since flags are considered optional by default
          not repeatable   -- mind repeating args (v0.7.0)
        )
  = setRepeatableOr x repeatable

-- [[-a]] -> [-a]
canonicaliseArg (Group (x@{
    branches: ((Group y) : Nil) : Nil
  , optional
  , repeatable
  }))
  = canonicaliseArg (Group (y {
                      optional   = y.optional   || x.optional
                    , repeatable = y.repeatable || x.repeatable
                    }))

-- [[-a] | [-b]] -> [-a | -b]
canonicaliseArg (Group (x@{
    branches: branches
  , optional
  , repeatable
  }))
  = Group (x { branches = branches' })
    where
      branches' = do
        branch <- branches
        pure case branch of
          arg : Nil ->
            case canonicaliseArg arg of
                  Group (y@{ branches: branch : Nil })
                    | y.optional   == optional &&
                      y.repeatable == repeatable -> branch
                  a -> pure a
          b -> canonicaliseArg <$> b

canonicaliseArg a = a
