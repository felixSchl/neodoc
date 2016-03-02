module Language.Docopt.Parser.Usage.Usage (
    Usage (..)
  , prettyPrintUsage
  ) where

import Prelude
import Data.List (List())
import Data.Either (Either())
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(), maybe)
import Control.Bind ((=<<))

import Language.Docopt.Parser.Usage.Argument as U
import Language.Docopt.Parser.Lexer          as L
import Language.Docopt.Parser.Usage.Option   as O

-- | Represent a single program usage.
-- | A single usage is made up of a list of mutually exclusive groups,
-- | separated by a vertical bar `|`. Each of those groups can contain
-- | one or more `Argument`.
-- |
-- | node node | node | node
-- | ^^^^ ^^^^   ^^^^   ^^^^
-- |   |   |      |      |
-- | [ 0 , 1 ]  [ 0 ]  [ 0 ]
-- |    \ /       |      |
-- | [   0    ,   1   ,  2 ]
data Usage = Usage String (List U.Branch)

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

instance eqUsage :: Eq Usage where
  eq (Usage n xs) (Usage n' xs') = (n == n') && (xs == xs')

prettyPrintUsage :: Usage -> String
prettyPrintUsage (Usage name bs) =
  name ++ " " ++ intercalate " | " (U.prettyPrintBranch <$> bs)
