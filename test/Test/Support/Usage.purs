module Test.Support.Usage where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), length, (!!), take, toList)

import Language.Docopt
import qualified Language.Docopt.Parser.Usage          as U
import qualified Language.Docopt.Parser.Usage.Argument as U
import qualified Language.Docopt.Parser.Usage.Option   as O
import qualified Language.Docopt.Parser.Lexer          as Lexer
import qualified Language.Docopt.Scanner               as Scanner
import Language.Docopt.Parser.Base (debug)
import Text.Wrap (dedent)

-- short hand to create a usage
usage :: String -> Array (Array U.Argument) -> U.Usage
usage n xss = U.Usage n $ toList $ toList <$> xss

-- short hand to create a required group node
gr :: Array (Array U.Argument) -> Boolean -> U.Argument
gr xs r = U.Group false ls r
  where ls = toList <$> (toList xs)

-- short hand to create a optional group node
go :: Array (Array U.Argument) -> Boolean -> U.Argument
go xs r = U.Group true ls r
  where ls = toList <$> (toList xs)

ref            = U.Reference
eoa            = U.EOA
stdin          = U.Stdin
co n           = U.Command n false
po n           = U.Positional n false
poR n          = U.Positional n true
po' n          = U.Positional n
arg'  n o      = { name: n, optional: o }
arg_  n        = arg' n true
arg   n        = arg' n false

sopt'  f fs a r = U.OptionStack { flag: f, stack: fs, arg: a, repeatable: r }
sopt   f fs a   = sopt' f fs (pure a) false
sopt_  f fs     = sopt' f fs Nothing false
soptR  f fs a   = sopt' f fs (pure a) true
soptR_ f fs     = sopt' f fs Nothing true
lopt'  n    a r = U.Option { name: n, arg: a, repeatable: r }
lopt   n    a   = lopt' n (pure a) false
lopt_  n        = lopt' n Nothing false
loptR  n    a   = lopt' n (pure a) true
loptR_ n        = lopt' n Nothing true
