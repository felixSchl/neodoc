module Language.Docopt.Usage (
    Program ()
  , Usage (..)
  , runUsage
  , prettyPrintUsage
  ) where

import Prelude
import Data.List (List(..))
import Data.Monoid (class Monoid)
import Data.Foldable (intercalate)
import Language.Docopt.Argument (Branch, prettyPrintBranch)

type Program = List Usage
newtype Usage = Usage (List Branch)

runUsage :: Usage -> List Branch
runUsage (Usage x) = x

instance showUsage :: Show Usage where
  show (Usage xs) = "Usage " <> show (show <$> xs)

instance eqUsage :: Eq Usage where
  eq (Usage xs) (Usage ys) = xs == ys

instance semigroupUsage :: Semigroup Usage where
  append (Usage xs) (Usage ys) = Usage (xs <> ys)

instance monoidUsage :: Monoid Usage where
  mempty = Usage Nil

prettyPrintUsage :: Usage -> String
prettyPrintUsage (Usage xs)
  = intercalate " | " (prettyPrintBranch <$> xs)
