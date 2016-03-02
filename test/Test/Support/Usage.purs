module Test.Support.Usage where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), length, (!!), take, toList)

import Language.Docopt
import qualified Language.Docopt.Parser.Usage          as U
import qualified Language.Docopt.Parser.Usage.Argument as U
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
