module Language.Docopt.Usage (
    Program ()
  , Usage (..)
  , prettyPrintUsage
  ) where

import Prelude
import Data.List (List(..))
import Data.Monoid (Monoid)
import Data.Foldable (intercalate)
import Language.Docopt.Argument

type Program = List Usage
newtype Usage = Usage (List Branch)

instance showUsage :: Show Usage where
  show (Usage xs) = "Usage " ++ show (show <$> xs)

instance eqUsage :: Eq Usage where
  eq (Usage xs) (Usage ys) = xs == ys

instance semigroupUsage :: Semigroup Usage where
  append (Usage xs) (Usage ys) = Usage (xs <> ys)

instance monoidUsage :: Monoid Usage where
  mempty = Usage Nil

prettyPrintUsage :: Usage -> String
prettyPrintUsage (Usage xs)
  = intercalate " | " (prettyPrintBranch <$> xs)


