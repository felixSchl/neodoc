module Test.Support.Usage where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), length, (!!), take, toList)

import Language.Docopt
import qualified Language.Docopt.Parser.Usage as Usage
import qualified Language.Docopt.Parser.Lexer as Lexer
import qualified Language.Docopt.Parser.Scanner as Scanner
import Language.Docopt.Parser.Base (debug)
import Text.Wrap (dedent)

-- short hand to create a usage
usage :: String -> Array (Array Usage.Argument) -> Usage.Usage
usage n xss = Usage.Usage n $ toList $ toList <$> xss

-- short hand to create a command node
co :: String -> Usage.Argument
co = Usage.Command

-- short hand to create a short option node
so :: Char -> Array Char -> Maybe String -> Boolean -> Usage.Argument
so = Usage.OptionStack

-- short hand to create a long option node
lo :: String -> Maybe String -> Boolean -> Usage.Argument
lo = Usage.Option

-- short hand to create a positional node
po :: String -> Boolean -> Usage.Argument
po = Usage.Positional

-- short hand to create a required group node
gr :: Array (Array Usage.Argument) -> Boolean -> Usage.Argument
gr xs r = Usage.Group false ls r
  where ls = toList <$> (toList xs)

-- short hand to create a optional group node
go :: Array (Array Usage.Argument) -> Boolean -> Usage.Argument
go xs r = Usage.Group true ls r
  where ls = toList <$> (toList xs)

-- short hand to create an end-of-argument marker
eoa :: Usage.Argument
eoa = Usage.EOA
