module Neodoc.ArgParser.Arg where

import Prelude
import Data.Maybe
import Data.Function (on)
import Data.Generic.Rep
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
        Boolean           -- is this argument optional?
type ArgLayout = Layout Arg

derive instance genericArg :: Generic Arg

instance showArg :: Show Arg where
  show = gShow

instance prettyArg :: Pretty Arg where
  pretty (Arg i a _ _ o) =
    "#" <> show i <> ":"
        <> (if o then "[" else "")
        <> pretty a
        <> (if o then "]" else "")

instance eqArg :: Eq Arg where
  eq = eq `on` getId

getArg :: Arg -> SolvedLayoutArg
getArg (Arg _ x _ _ _) = x

getArgKey :: Arg -> ArgKey
getArgKey (Arg _ _ k _ _) = k

getId :: Arg -> Id
getId (Arg i _ _ _ _) = i

setId :: Id -> Arg -> Arg
setId i (Arg _ a k mV o) = Arg i a k mV o

isOptional :: Arg -> Boolean
isOptional (Arg _ _ _ _ o) = o

getFallback :: Arg -> Maybe RichValue
getFallback (Arg _ _ _ mV _) = mV

setRepeatable :: Boolean -> ArgLayout -> ArgLayout
setRepeatable b (Group o _ xs) = Group o b xs
setRepeatable b (Elem x) = Elem $ setElemRepeatable b x

setElemRepeatable :: Boolean -> Arg -> Arg
setElemRepeatable b (Arg i x k mV o) = Arg i (Solved.setElemRepeatable b x) k mV o

isFreeLayout :: ArgLayout -> Boolean
isFreeLayout (Elem (Arg _ x _ _ _)) = Solved.isFreeElem x
isFreeLayout (Group _ _ xs) = all (all isFreeLayout) xs

isFree :: Arg -> Boolean
isFree (Arg _ x _ _ _) = Solved.isFreeElem x

isRepeatable :: ArgLayout ->  Boolean
isRepeatable (Elem x) = isArgRepeatable x
isRepeatable (Group _ r _) = r

isArgRepeatable :: Arg -> Boolean
isArgRepeatable (Arg _ x _ _ _) = Solved.isElemRepeatable x

isOptionElem :: ArgLayout -> Boolean
isOptionElem (Elem x) = isOption x
isOptionElem _ = false

isOption :: Arg -> Boolean
isOption (Arg _ x _ _ _) = Solved.isOption x
