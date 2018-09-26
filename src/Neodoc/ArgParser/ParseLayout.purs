module Neodoc.ArgParser.ParseLayout where

import Prelude
import Data.NonEmpty (NonEmpty)
import Data.Function (on)
import Data.Generic.Rep
import Data.Pretty
import Data.List (List)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Neodoc.ArgParser.Arg as Arg
import Neodoc.ArgParser.Arg hiding (getId)

{-
A recursive layout structure suited for parsing.

Each nested grouping indicates if it is ought to be considered "fixed", or
"free" in terms of the groups' occurence rules. The rules are simple:
Groups that contain any branch that contains any positional argument are now
"fixed". This is restriction is set in place in order to allow re-evaluating
the group in face of repetition:

"u: (-a | -b)..." says that the group *in it's entirety* should be able to occur
one or more times. This means that all of these inputs are valid: "-ab", "-bb",
"-abab", "-abbbab".

An alternative contemplation was to "lock" the branch after the initial match.
This, in effect, alters the spec "on-the-fly" for this pattern's evaluation.
This would cause "u: (-a | b)..." to only match inputs: "-a", "-aa", ..., "-b",
"-bb" and so on, mixing "a"s and "b"s would be considered a failure. Neodoc
does not yet implement such a pattern because it is less general. The above
could also be achieved, more flexibly, using "u: (-a...|-b...)".

Secondly, this data structure presents and "id", suitable for quick cache
lookups and identification of elements later on.
-}
type ParseBranch a = NonEmpty List (ParseLayout a)
data ParseLayout a
  = ParseElem
      Int     -- id
      Boolean -- free?
      a
  | ParseGroup
      Int     -- id
      Boolean -- free?
      Boolean -- optional?
      Boolean -- repeatable?
      (NonEmpty List (ParseBranch a))
type ArgParseLayout = ParseLayout Arg

getElem :: ∀ a. ParseLayout a -> Maybe a
getElem (ParseElem _ _ x) = Just x
getElem _ = Nothing

getId :: ∀ a. ParseLayout a -> Int
getId (ParseElem id _ _) = id
getId (ParseGroup id _ _ _ _) = id

derive instance genericParseLayout :: (Generic a) => Generic (ParseLayout a)
instance showParseLayout :: (Generic a, Show a) => Show (ParseLayout a) where
  show = gShow

instance eqParseLayout :: Eq (ParseLayout a) where
  eq = eq `on` getId

instance prettyParseLayout :: (Pretty a) => Pretty (ParseLayout a) where
  pretty (ParseElem id free x)
    = "E(" <> show id <> ":"
        <> (if free then "<*" else "<!")
        <> pretty x
        <> (if free then "*>" else "!>")
        <> ")"
  pretty (ParseGroup id free o r xs)
    = "G"
        <> (if o then "[" else "(")
        <> show id <> ":"
        <> (if free then "<*" else "<!")
        <> (intercalate "|" (pretty <$> xs))
        <> (if free then "*>" else "!>")
        <> (if o then "]" else ")")
        <> (if r then "..." else "")
