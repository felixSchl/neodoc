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
sopt :: Char -> Array Char -> String -> Usage.Argument
sopt f fs a = Usage.OptionStack f fs (pure a) false

sopt_ :: Char -> Array Char -> Usage.Argument
sopt_ f fs = Usage.OptionStack f fs Nothing false

soptR :: Char -> Array Char -> String -> Usage.Argument
soptR f fs a = Usage.OptionStack f fs (pure a) true

soptR_ :: Char -> Array Char -> Usage.Argument
soptR_ f fs = Usage.OptionStack f fs Nothing true

-- short hand to create a long option node
lopt :: String -> String -> Usage.Argument
lopt n a = Usage.Option n (pure a) false

lopt_ :: String -> Usage.Argument
lopt_ n = Usage.Option n Nothing false

loptR :: String -> String -> Usage.Argument
loptR n a = Usage.Option n (pure a) true

loptR_ :: String -> Usage.Argument
loptR_ n = Usage.Option n Nothing true

-- short hand to create a positional node
po :: String -> Usage.Argument
po n = Usage.Positional n false

poR :: String -> Usage.Argument
poR n = Usage.Positional n true

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
