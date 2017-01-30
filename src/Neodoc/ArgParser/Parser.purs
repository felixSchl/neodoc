module Neodoc.ArgParser.Parser where

import Prelude

import Debug.Trace

import Data.List (List(..), (:), fromFoldable, toUnfoldable, concat, singleton, any)
import Data.Maybe
import Data.Pretty
import Data.String as String
import Data.String.Ext as String
import Data.Array.Partial as AU
import Data.Foldable (foldl, elem)
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\))
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NE
import Data.Either (Either(..))
import Data.Traversable (for, traverse)
import Data.Function.Memoize
import Partial.Unsafe

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.MonadZero (guard)
import Control.Monad.State
import Control.Monad.State as State

import Neodoc.Value.Origin (Origin(..))
import Neodoc.Value.Origin as Origin
import Neodoc.OptionAlias as OA
import Neodoc.Data.OptionArgument (OptionArgument(..))
import Neodoc.Env
import Neodoc.Data.Layout
import Neodoc.Data.Description
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Value
import Neodoc.Value as Value
import Neodoc.Value.RichValue
import Neodoc.Value.RichValue as RichValue
import Neodoc.ArgKey.Class
import Neodoc.Spec (Spec(..), Toplevel)
import Neodoc.Parsing.Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.Parsing.Parser.Combinators
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Type as ArgParser
import Neodoc.ArgParser.Arg
import Neodoc.ArgParser.Arg as Arg
import Neodoc.ArgParser.Fallback
import Neodoc.ArgParser.Evaluate (chooseBest)
import Neodoc.ArgParser.Result
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Pattern hiding (PatternError(..))
import Neodoc.ArgParser.Pattern (PatternError)
import Neodoc.ArgParser.Pattern as Pattern
import Neodoc.ArgParser.Token hiding (Token(..))
import Neodoc.ArgParser.Token (Token)
import Neodoc.ArgParser.Token as Tok
import Neodoc.ArgParser.KeyValue

{-
  Match a single argument against input
-}
match
  :: (Token -> Boolean)
  -> Arg
  -> List PositionedToken
  -> AllowOmissions
  -> PatternMatch PositionedToken ArgParseError KeyValue
match isKnownToken arg is allowOmissions =
  let a = Arg.getArg arg
      argv = fromArgv a is
      fallback = fromFallback a (Arg.getFallback arg)
   in argv <|> fallback

  where

  fail' = Left <<< (false /\ _)
  fail = fail' <<< ArgParser.GenericError
  fatal' = Left <<< (true /\ _)
  fatal = fatal' <<< ArgParser.GenericError

  expected arg = case is of
    ptok@(PositionedToken tok _ _) : _ -> do
      if isKnownToken tok
         then fail' $ unexpectedInputError $ known ptok
         else fail' $ unexpectedInputError $ unknown ptok
    Nil -> fail' $ missingArgumentError arg

  fromArgv = go
    where
    return is = Right <<< (_ /\ is)
                      <<< (arg /\ _)
                      <<< RichValue.from Origin.Argv

    go arg Nil = expected arg

    go (Command n _) ((PositionedToken (Tok.Lit s) _ _):is)
      | n == s
      = return is $ BoolValue true

    go (Positional _ _) ((PositionedToken (Tok.Lit s) _ _):is)
      = return is $ StringValue s

    go EOA ((PositionedToken (Tok.EOA xs) _ _):is)
      = return is $ ArrayValue (toUnfoldable xs)

    go Stdin ((PositionedToken Tok.Stdin _ _):is)
      = return is $ BoolValue true

    go (arg@(Option a@(OA.Long n) mA _)) ((PositionedToken (Tok.LOpt n' mA') _ _):is)
      | String.startsWith n' n
      = case mA /\ mA' of
          Nothing /\ Just _ | n == n' ->
            fatal $ "Option does not take arguments: " <> pretty a
          Nothing /\ Nothing | n == n' -> return is $ BoolValue true
          Just (OptionArgument _ o) /\ _ ->
            let explicit = do
                  guard $ n' == n
                  (_ /\ is) <<< StringValue <$> mA'
                adjacent = do
                  guard $ n' == n
                  case is of
                    (PositionedToken (Tok.Lit s) _ _):is' ->
                      pure (StringValue s /\ is')
                    _ -> Nothing
                subsume = do
                  v <- String.stripPrefix (String.Pattern n) n'
                  pure (StringValue v /\ is)
             in case explicit <|> adjacent <|> subsume of
                  Nothing | not o ->
                    fatal $ "Option requires argument: " <> pretty a
                  Nothing -> return is $ BoolValue true
                  Just (v /\ is) -> return is v
          _ -> expected arg

    go (arg@(Option a@(OA.Short f) mA _)) ((PositionedToken (Tok.SOpt f' xs mA') _ _):is)
      | f == f'
      = let adjacent = case is of
              (PositionedToken (Tok.Lit s) _ _):is' ->
                pure (StringValue s /\ is')
              _ -> Nothing
         in case mA /\ xs /\ mA' of
              Just _ /\ [] /\ (Just s) ->
                return is $ StringValue s
              Just (OptionArgument _ o) /\ [] /\ Nothing ->
                case adjacent of
                  Just (v /\ is) -> return is v
                  Nothing | o -> return is $ BoolValue true
                  Nothing  -> fatal' $ optionRequiresArgumentError (OA.Short f)
              Nothing /\ [] /\ (Just _) ->
                fatal' $ optionTakesNoArgumentError (OA.Short f)
              Nothing /\ xs /\ mA' ->
                let newTok = Tok.SOpt (unsafePartial $ AU.head xs)
                                      (unsafePartial $ AU.tail xs)
                                      mA'
                 in return (newTok <> is) $ BoolValue true

    go arg _ = expected arg

  fromFallback arg _ | not allowOmissions = expected arg
  fromFallback arg Nothing = expected arg
  fromFallback _ (Just v) = Right $ ((arg /\ v) /\ is)

lowerError
  :: (Token -> Boolean)
  -> PatternError PositionedToken Arg
  -> ArgParseError
lowerError isKnownToken = case _ of
  Pattern.GenericError s -> GenericError s
  Pattern.UnexpectedInputError (ptok@(PositionedToken tok _ _):|_) ->
    unexpectedInputError $ if isKnownToken tok
                                then known ptok
                                else unknown ptok
  Pattern.MissingPatternError p -> GenericError "???" -- TODO

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) ArgParseResult
parse (spec@(Spec { layouts, descriptions })) options env tokens =
  let toplevels = concat $ NE.toList layouts
      parsers =
        toplevels <#> \branch ->
          let leafs = layoutToPattern <$> NE.toList branch
              isKnownToken' = isKnownToken spec
           in do
              vs <- Pattern.parse (match isKnownToken')
                                  (lowerError isKnownToken')
                                  (toArgs options env descriptions leafs)
              pure $ ArgParseResult (Just branch) vs
   in do
    runParser { env, options, spec } {} {} tokens do
      chooseBest
        (\_ -> 1)
        (\_ -> 1)
        (parsers)

{-
  Determine if a given token is considered known
-}
isKnownToken
  :: Spec SolvedLayout
  -> Token
  -> Boolean
isKnownToken (Spec { layouts, descriptions }) = memoize go
  where
  go tok = occuresInDescs || occuresInLayouts
    where
    occuresInDescs = any matchesDesc descriptions
      where
      matchesDesc (OptionDescription as _ _ _ _) = test tok
        where
        test (Tok.LOpt n _)   = elem (OA.Long n) as
        test (Tok.SOpt s _ _) = elem (OA.Short s) as
        test _ = false
      matchesDesc _ = false
    occuresInLayouts = any (any (any matchesLayout)) layouts
      where
      matchesLayout (Group _ _ xs) = any (any matchesLayout) xs
      matchesLayout (Elem x) = test tok x
        where
        test (Tok.LOpt n _)   (Solved.Option a _ _) = OA.Long n == a
        test (Tok.SOpt s _ _) (Solved.Option a _ _) = OA.Short s == a
        test (Tok.Lit n)      (Solved.Command n' _) = n == n'
        test (Tok.EOA _)      (Solved.EOA)          = true
        test (Tok.Stdin)      (Solved.Stdin)        = true
        test _ _ = false

{-
  Convert a list of patterns containing solved layout arguments into a list of
  patterns containing pre-cached `Arg`s.
-}
toArgs
  :: ∀ r
   . Options r
  -> Env
  -> List Description
  -> List (Pattern SolvedLayoutArg)
  -> List (Pattern Arg)
toArgs options env descriptions xs = evalState (for xs go) 0
  where
  nextId = State.get <* State.modify (_ + 1)
  go (LeafPattern o r fix x) = nextId <#> LeafPattern o r fix <<< toArg x
  go (ChoicePattern o r fix xs) = ChoicePattern o r fix <$> for xs (traverse go)
  toArg x id =
    let mDesc = case x of
          Option alias _ _ -> findDescription alias descriptions
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
  -> Pattern SolvedLayoutArg

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
