module Neodoc.ArgParser.Parser where

import Prelude
import Data.List (List(..), fromFoldable, concat)
import Data.NonEmpty.Extra as NE
import Data.Either (Either(..))

import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.SolvedLayout (SolvedLayout)
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Spec (Spec(..), Toplevel)
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.Parsing.Parser.Combinators
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Result
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Pattern
import Neodoc.ArgParser.Pattern as Pattern
import Neodoc.ArgParser.Token

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) _ -- ArgParseResult
parse (spec@(Spec { layouts, descriptions })) options env tokens =
  let toplevels = NE.toList <$> do
                    concat $ NE.toList layouts
      patterns = (layoutToPattern <$> _) <$> toplevels
   in runParser { env, options, spec } {} {} tokens do
        Pattern.parse parseToken (Nil :: List (Pattern Boolean))

parseToken _ = Parser.fail "..."

{-
    Convert a layout into a "pattern" for the pattern parser to consume
-}

layoutToPattern
  :: SolvedLayout
  -> Pattern Unit

layoutToPattern (Elem x) = case x of
  Solved.Command    n r -> LeafPattern false r     true  unit
  Solved.Positional n r -> LeafPattern false r     true  unit
  Solved.Option  a mA r -> LeafPattern false r     false unit
  Solved.EOA            -> LeafPattern false false false unit
  Solved.Stdin          -> LeafPattern false false false unit

-- TODO: detect `isFixed`. This can be done in a separate pass, lazily (stop
--       recursing as soon as first fixed arg is found.
layoutToPattern (Group o r xs) = ChoicePattern o r false do
  NE.toList do
    ((layoutToPattern <$> _) <<< NE.toList) <$> do
      xs
