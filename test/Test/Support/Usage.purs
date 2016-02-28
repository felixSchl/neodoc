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

-- short hand to create a command node
co :: String -> U.Argument
co = U.Command

-- short hand to create a short option node
sopt :: Char -> Array Char -> String -> U.Argument
sopt f fs a = U.OptionStack f fs (pure a) false

sopt_ :: Char -> Array Char -> U.Argument
sopt_ f fs = U.OptionStack f fs Nothing false

soptR :: Char -> Array Char -> String -> U.Argument
soptR f fs a = U.OptionStack f fs (pure a) true

soptR_ :: Char -> Array Char -> U.Argument
soptR_ f fs = U.OptionStack f fs Nothing true

-- short hand to create a long option node
lopt :: String -> String -> U.Argument
lopt n a = U.Option n (pure a) false

lopt_ :: String -> U.Argument
lopt_ n = U.Option n Nothing false

loptR :: String -> String -> U.Argument
loptR n a = U.Option n (pure a) true

loptR_ :: String -> U.Argument
loptR_ n = U.Option n Nothing true

-- short hand to create a positional node
po :: String -> U.Argument
po n = U.Positional n false

poR :: String -> U.Argument
poR n = U.Positional n true

-- short hand to create a required group node
gr :: Array (Array U.Argument) -> Boolean -> U.Argument
gr xs r = U.Group false ls r
  where ls = toList <$> (toList xs)

-- short hand to create a optional group node
go :: Array (Array U.Argument) -> Boolean -> U.Argument
go xs r = U.Group true ls r
  where ls = toList <$> (toList xs)

-- short hand to create an end-of-argument marker
eoa :: U.Argument
eoa = U.EOA
