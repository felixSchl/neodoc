module Neodoc.ArgParser.Arg where

import Prelude
import Data.Maybe
import Data.Function (on)
import Data.Generic
import Data.Foldable (all)
import Data.Pretty
import Neodoc.Data.Layout
import Neodoc.Value.RichValue
import Neodoc.ArgKey
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
  = Arg Id                -- the unique id
        SolvedLayoutArg   -- the wrapped arg
        ArgKey            -- the (cached) arg key
        (Maybe RichValue) -- the fallback value for this argument
type ArgLayout = Layout Arg

derive instance genericArg :: Generic Arg

instance showArg :: Show Arg where
  show = gShow

instance eqArg :: Eq Arg where
  eq = eq `on` getId

instance prettyArg :: Pretty Arg where
  pretty (Arg i a _ _) = "#" <> show i <> ":" <> pretty a

getArg :: Arg -> SolvedLayoutArg
getArg (Arg _ x _ _) = x

getKey :: Arg -> ArgKey
getKey (Arg _ _ k _) = k

getId :: Arg -> Id
getId (Arg i _ _ _) = i

getFallback :: Arg -> Maybe RichValue
getFallback (Arg _ _ _ mV) = mV
