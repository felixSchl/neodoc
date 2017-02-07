module Neodoc.ArgParser.Arg where

import Prelude
import Data.Maybe
import Data.Function (on)
import Data.Generic
import Data.Foldable (all)
import Data.Pretty
import Data.Lazy (defer)
import Neodoc.Data.Layout
import Neodoc.Data.Description
import Neodoc.Value.RichValue
import Neodoc.ArgKey
import Data.Function.Memoize
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved

{-
  An argument data structure optimized for parsing.
  The idea is to cache as much as possible up-front,
  to avoid unnecessary traversals during the "hot"
  phases of the parse (i.e. permutation parsing).
 -}
type Id = Int
data Arg
  = Arg Id                  -- the unique id
        SolvedLayoutArg     -- the wrapped arg
        ArgKey              -- the (cached) arg key
        Boolean             -- can terminate parse?
        (Maybe Description) -- the description, if any
        (Maybe RichValue)   -- the fallback value for this argument
type ArgLayout = Layout Arg

derive instance genericArg :: Generic Arg

instance showArg :: Show Arg where
  show = gShow

instance ordArg :: Ord Arg where
  compare = gCompare

instance eqArg :: Eq Arg where
  eq = eq `on` getId

instance prettyArg :: Pretty Arg where
  pretty (Arg i a _ _ _ _) = "#" <> show i <> ":" <> pretty a

getArg :: Arg -> SolvedLayoutArg
getArg (Arg _ x _ _ _ _) = x

getKey :: Arg -> ArgKey
getKey (Arg _ _ k _ _ _) = k

getId :: Arg -> Id
getId (Arg i _ _ _ _ _) = i

getDescription :: Arg -> Maybe Description
getDescription (Arg _ _ _ _ mD _) = mD

getFallback :: Arg -> Maybe RichValue
getFallback (Arg _ _ _ _ _ mV) = mV

hasFallback :: Arg -> Boolean
hasFallback (Arg _ _ _ _ _ (Just _)) = true
hasFallback _ = false

canTerm :: Arg -> Boolean
canTerm (Arg _ _ _ c _ _) = c
