module Language.Docopt.Parser.Usage.Usage (
    Usage (..)
  , prettyPrintUsage
  ) where

import Prelude
import Data.List (List())
import Data.Foldable (intercalate)

import Language.Docopt.Parser.Usage.Argument as U
data Usage = Usage String (List U.Branch)

instance showUsage :: Show Usage where
  show (Usage n xs) = "Usage " ++ show n ++ " " ++ show xs

instance eqUsage :: Eq Usage where
  eq (Usage n xs) (Usage n' xs') = (n == n') && (xs == xs')

prettyPrintUsage :: Usage -> String
prettyPrintUsage (Usage name bs) =
  name ++ " " ++ intercalate " | " (U.prettyPrintBranch <$> bs)
