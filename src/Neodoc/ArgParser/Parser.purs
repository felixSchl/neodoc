module Neodoc.ArgParser.Parser where

import Prelude

import Debug.Trace

import Data.List (List(..), fromFoldable, concat, singleton, any)
import Data.Maybe
import Data.Tuple.Nested ((/\))
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NE
import Data.Either (Either(..))

import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.Description
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Value
import Neodoc.Value as Value
import Neodoc.Value.RichValue
import Neodoc.ArgKey.Class
import Neodoc.Spec (Spec(..), Toplevel)
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.Parsing.Parser.Combinators
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Arg
import Neodoc.ArgParser.Fallback
import Neodoc.ArgParser.Result
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Pattern
import Neodoc.ArgParser.Pattern as Pattern
import Neodoc.ArgParser.Token

-- match
--   :: _
--   -> List PositionedToken
--   -> AllowOmissions
--   -> PatternMatch PositionedToken ArgParseError _
-- -- match _ _ = Left $ false /\ (GenericError "...")
-- match _ _ _ = Right (unit /\ Nil)
--
parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) ArgParseResult
parse (spec@(Spec { layouts, descriptions })) options env tokens =
  let toplevels = NE.toList <$> (concat $ NE.toList layouts)
      patterns = singleton $ toArgs options env descriptions $ do
                  ChoicePattern false false true do
                    (layoutToPattern <$> _) <$>
                      toplevels
   in do
      -- xs <- runParser { env, options, spec } {} {} tokens do
      --   Pattern.parse match patterns
      -- traceShowA xs
      pure (ArgParseResult Nothing Nil)

toArgs
  :: ∀ f r
   . (Foldable f)
  => Options r
  -> Env
  -> List Description
  -> f SolvedLayoutArg
  -> f Arg
toArgs options env descriptions = 


toArg
  :: ∀ r
   . Options r
  -> Env
  -> List Description
  -> Int
  -> SolvedLayoutArg
  -> Arg
toArg options env descriptions id x =
  let mDesc = case x of
        (Option alias _ _) -> findDescription alias descriptions
        _ -> Nothing
      fallback = do
        v <- unRichValue <$> getFallbackValue options env mDesc x
        pure $ RichValue v {
          value = if Solved.isElemRepeatable x
                    then ArrayValue $ Value.intoArray v.value
                    else v.value
        }
    in Arg id x (toArgKey x) mDesc fallback

{-
  Convert a layout into a "pattern" for the pattern parser to consume
-}
layoutToPattern
  :: SolvedLayout
  -> Pattern _

layoutToPattern (Elem x) = case x of
  Solved.Command    n r -> LeafPattern false r     true  x
  Solved.Positional n r -> LeafPattern false r     true  x
  Solved.Option  a mA r -> LeafPattern false r     false x
  Solved.EOA            -> LeafPattern false false false x
  Solved.Stdin          -> LeafPattern false false false x

layoutToPattern (Group o r xs) =
  let xs' = NE.toList do
              ((layoutToPattern <$> _) <<< NE.toList) <$> do
                xs
      fix = any (any isFixed) xs'
   in ChoicePattern o r fix xs'
