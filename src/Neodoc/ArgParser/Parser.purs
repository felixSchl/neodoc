module Neodoc.ArgParser.Parser where

import Prelude

import Debug.Trace

import Data.List (List(..), (:), fromFoldable, toUnfoldable, concat, singleton, any, null, filter)
import Data.List.Extra (spanMap)
import Data.Maybe
import Data.Bifunctor (rmap, lmap)
import Data.Pretty
import Data.String as String
import Data.String.Ext as String
import Data.Array.Partial as AU
import Data.Array as A
import Data.Foldable (foldl, elem, all)
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\))
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
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
import Neodoc.Parsing.Parser.Combinators as Parser
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Type as ArgParser
import Neodoc.ArgParser.Arg
import Neodoc.ArgParser.Arg as Arg
import Neodoc.ArgParser.Fallback
import Neodoc.ArgParser.Result
import Neodoc.ArgParser.Options
import Neodoc.ArgParser.Pattern hiding (PatternError(..))
import Neodoc.ArgParser.Pattern (PatternError)
import Neodoc.ArgParser.Pattern as Pattern
import Neodoc.ArgParser.Token hiding (Token(..))
import Neodoc.ArgParser.Token (Token)
import Neodoc.ArgParser.Token as Tok
import Neodoc.ArgParser.KeyValue

-- An artificial argument to be injected to capture any unknown options
_UNKNOWN_ARG :: Arg
_UNKNOWN_ARG =
  let arg = Solved.Command "?" true
      key = toArgKey arg
   in Arg (-1) arg key false Nothing Nothing

-- An artificial argument to be injected to capture any '--', regardless
-- of whether it occurs in the spec or not
_EOA :: Arg
_EOA =
  let arg = Solved.EOA
      key = toArgKey arg
   in Arg (-2) arg key false Nothing Nothing

{-
  Match a single argument against input
-}
match
  :: (Token -> Boolean) -- is this token known?
  -> Boolean            -- allow unknown?
  -> Arg
  -> List PositionedToken
  -> AllowOmissions
  -> Match PositionedToken ArgParseError KeyValue
match isKnownToken allowUnknown arg is allowOmissions =
  let a = Arg.getArg arg
      argv = fromArgv (Arg.canTerm arg) a is
      fallback = fromFallback a (Arg.getFallback arg)
   in ((arg /\ _) <$> (argv <|> fallback)) <|> unknown

  where

  fail' = Failed false
  fail = Failed false <<< ArgParser.GenericError
  fatal' = Failed true
  fatal = Failed true <<< ArgParser.GenericError

  unknown = case is of
    -- if all fails, try to see if we can consume this input as an "unknown"
    -- option
    ptok@(PositionedToken tok src _) : is ->
      if isKnownToken tok
        then NoMatch
        else if allowUnknown
          then case tok of
            Tok.Lit _ ->
              NoMatch
            Tok.EOA xs -> Success true is do
              (_EOA /\ (RichValue.from Origin.Argv $ ArrayValue $ A.fromFoldable xs))
            _ -> Success true is do
              (_UNKNOWN_ARG /\ (RichValue.from Origin.Argv $ StringValue src))
          else NoMatch
    _ -> NoMatch

  fromArgv canTerm a is = RichValue.from Origin.Argv <$> go a is
    where

    terminate is = Terminated do
      ArrayValue $ A.fromFoldable $ StringValue <<< Tok.getSource <$> is

    go arg Nil = NoMatch

    go (Command n _) ((PositionedToken (Tok.Lit s) _ _):is)
      | n == s
      = if canTerm
          then terminate is
          else Success false is $ BoolValue true

    go (Positional _ _) ((PositionedToken (Tok.Lit s) _ _):is)
      = Success false is $ StringValue s

    go EOA ((PositionedToken (Tok.EOA xs) _ _):is)
      = Terminated $ ArrayValue (toUnfoldable xs)

    go Stdin ((PositionedToken Tok.Stdin _ _):is)
      = Success false is $ BoolValue true

    go (Option a mA r) toks
      = let aliases = case Arg.getDescription arg of
              Just (OptionDescription aliases _ _ _ _) -> aliases
              _ -> NE.singleton a
         in NE.foldl1 (<|>) $ opt toks mA r <$> aliases

    go arg _ = NoMatch

    opt ((PositionedToken (Tok.LOpt n' mA') _ _):is) mA r (a@(OA.Long n))
      | String.startsWith n n'
      = case mA /\ mA' of
          Nothing /\ Just _ | n == n' ->
            fatal' $ optionTakesNoArgumentError (OA.Long n)
          Nothing /\ Nothing | n == n' ->
            if canTerm
               then terminate is
               else Success false is $ BoolValue true
          Just (OptionArgument _ o) /\ _ ->
            let explicit = do
                  guard $ n' == n
                  (_ /\ is) <<< StringValue <$> mA'
                adjacent = do
                  guard $ n' == n
                  case is of
                    (PositionedToken (Tok.Lit s) _ _):is' ->
                      if r
                        then do
                          ss /\ is'' <- pure $
                            lmap A.fromFoldable $
                              flip spanMap is' case _ of
                                PositionedToken (Tok.Lit s) _ _ -> Just s
                                _ -> Nothing
                          pure $ (ArrayValue $ StringValue <$> (s A.: ss)) /\ is''
                        else pure $ StringValue s /\ is'
                    _ -> Nothing
                subsume = do
                  v <- String.stripPrefix (String.Pattern n) n'
                  v' <- if String.null v
                            then if o
                                    then pure $ BoolValue true
                                    else if canTerm
                                            then pure $ ArrayValue []
                                            else Nothing
                            else pure $ StringValue v
                  pure (v' /\ is)

             in case explicit <|> adjacent <|> subsume of
                  Nothing | canTerm -> terminate is
                  Just (v /\ is') | canTerm ->
                    let v' = ArrayValue $ A.fromFoldable $ StringValue <<< Tok.getSource <$> is'
                        v'' = ArrayValue $ Value.intoArray v <> Value.intoArray v'
                      in Terminated v''
                  Nothing | not o ->
                    fatal' $ optionRequiresArgumentError (OA.Long n)
                  Nothing -> Success false is $ BoolValue true
                  Just (v /\ is) -> Success false is v
          _ -> NoMatch

    opt ((PositionedToken (Tok.SOpt f' xs mA') src _):is) mA r (a@(OA.Short f))
      | f == f'
      = let adjacent = case is of
              (PositionedToken (Tok.Lit s) _ _):is' ->
                if r
                  then do
                    ss /\ is'' <- pure $
                      lmap A.fromFoldable $
                        flip spanMap is' case _ of
                          PositionedToken (Tok.Lit s) _ _ -> Just s
                          _ -> Nothing
                    pure $ (ArrayValue $ StringValue <$> (s A.: ss)) /\ is''
                  else pure $ StringValue s /\ is'
              _ -> Nothing
         in case mA /\ xs /\ mA' of

              -- note: fail even for `canTerm` to retain current neodoc behavior
              Nothing /\ [] /\ (Just _) ->
                fatal' $ optionTakesNoArgumentError (OA.Short f)

              Just _ /\ xs /\ mA' | canTerm ->
                let rest = if A.null xs then "" else String.drop 2 src
                    v = maybe [] (A.singleton <<< StringValue) mA'
                    v' = if String.null rest then v else [ StringValue rest ]
                    v'' = ArrayValue $ A.fromFoldable $ StringValue <<< Tok.getSource <$> is
                    v''' = ArrayValue $ v' <> Value.intoArray v''
                 in Terminated v'''

              -- note: allow explict arg even when option does not take one,
              --       when `canTerm` is true.
              Nothing /\ [] /\ mA' | canTerm ->
                let v = maybe [] (A.singleton <<< StringValue) mA'
                    v' = ArrayValue $ A.fromFoldable $ StringValue <<< Tok.getSource <$> is
                    v'' = ArrayValue $ v <> Value.intoArray v'
                 in Terminated v''

              Just _ /\ [] /\ (Just s) ->
                Success false is $ StringValue s

              Just (OptionArgument _ o) /\ [] /\ Nothing ->
                case adjacent of
                  Just (v /\ is) -> Success false is v
                  Nothing | o -> Success false is $ BoolValue true
                  Nothing  -> fatal' $ optionRequiresArgumentError (OA.Short f)

              Just _ /\ xs /\ Nothing ->
                Success false is $ StringValue $ String.fromCharArray xs

              Nothing /\ [] /\ Nothing ->
                Success false is $ BoolValue true

              -- note: there's varying opinion here as to what should happen:
              --    -fb=oobar => either (a) -f => "b=oobar"
              --                        (b) fail! (since '-b' is using explicit arg)
              Just (OptionArgument _ false) /\ xs /\ Just _ | (not $ A.null xs) ->
                Success false is $ StringValue $ String.drop 2 src

              Just (OptionArgument _ true) /\ xs /\ _ | (not $ A.null xs) ->
                let newTok = Tok.SOpt (unsafePartial $ AU.head xs)
                                      (unsafePartial $ AU.tail xs)
                                      mA'
                    newSrc = "-" <> String.drop 2 src
                    newPtok = PositionedToken newTok newSrc (-1) {- TODO: how to get fresh id? -}
                 in Success false (newPtok : is) $ BoolValue true

              Nothing /\ xs /\ mA' | (not $ A.null xs) ->
                let newTok = Tok.SOpt (unsafePartial $ AU.head xs)
                                      (unsafePartial $ AU.tail xs)
                                      mA'
                    newSrc = "-" <> String.drop 2 src
                    newPtok = PositionedToken newTok newSrc (-1) {- TODO: how to get fresh id? -}
                 in Success false (newPtok : is) $ BoolValue true
              _ -> NoMatch

    opt _ _ _ _ = NoMatch

  fromFallback arg _ | not allowOmissions = NoMatch
  fromFallback arg Nothing = NoMatch
  fromFallback _ (Just v) = Substituted v

lowerError
  :: (Token -> Boolean)
  -> PatternError PositionedToken Arg
  -> ArgParseError
lowerError isKnownToken = case _ of
  Pattern.GenericError s -> GenericError s
  Pattern.UnexpectedInputError ptoks ->
    unexpectedInputError $ ptoks <#> \(ptok@(PositionedToken tok _ _)) ->
                                        if isKnownToken tok
                                          then known ptok
                                          else unknown ptok
  Pattern.MissingElementError a ->
    missingArgumentError (Arg.getArg a)
  Pattern.MissingPatternError x ->
    case getLeftMostElement x of
      Nothing -> GenericError ""
      Just a  -> missingArgumentError (Arg.getArg a)

  where
  getLeftMostElement (LeafPattern _ _ _ x) = Just x
  getLeftMostElement (ChoicePattern _ _ _ ((p:_):_)) = getLeftMostElement p
  getLeftMostElement (ChoicePattern _ _ _ _) = Nothing

parse
  :: ∀ r
   . Spec SolvedLayout
  -> Options r
  -> Env
  -> List PositionedToken
  -> Either (ParseError ArgParseError) ArgParseResult
parse (spec@(Spec { layouts, descriptions })) options env tokens =
  let hasEmpty = any null layouts
      toplevels = concat $ NE.toList layouts
      isKnownToken' = isKnownToken spec
      taggedPats = toplevels <#> (\branch ->
          let leafs = do
                layoutToPattern options.requireFlags
                                options.repeatableOptions <$> NE.toList branch
              argLeafs = concat $ simplifyLayout <$> toArgLeafs   options
                                                                  env
                                                                  descriptions
                                                                  leafs
           in branch /\ argLeafs
        )
   in do
    runParser { env, options, spec } {} {} tokens do
      mBranch /\ vs <- do
        Parser.choice $ Parser.try <$>
          let x = do
                branch /\ vs <- Pattern.parseBestTag
                  (match isKnownToken' options.allowUnknown)
                  (lowerError isKnownToken')
                  taggedPats
                vs' <- eof options.allowUnknown isKnownToken'
                Parser.return $ Just branch /\ (vs <> vs')
              y = if not hasEmpty then Nil else singleton $
                    let branch = emptyBranch options.allowUnknown
                      in do
                      vs <- eof options.allowUnknown isKnownToken'
                      pure $ branch /\ vs
           in x : y

      -- actually parse captured values into their respective types.
      -- note: previous version of neodoc would do this parse on-the-fly.
      let readRv rv =
            let readV = case _ of
                        StringValue v -> Value.read v false
                        ArrayValue vs -> ArrayValue $ readV <$> vs
                        v -> v
             in RichValue.setValue (readV $ _.value $ unRichValue rv) rv

          -- inject the pseudo argument to collect unknown options into layout
          -- so that the value reduction will work.
          mOutBranch = case mBranch of
            Nothing -> mBranch
            Just branch -> Just do
              if options.allowUnknown
                then NE.append (Elem <<< Arg.getArg <$> do
                        _UNKNOWN_ARG :| _EOA : Nil
                      ) branch
                else branch
      pure $ ArgParseResult mOutBranch (rmap readRv <$> vs)

  where
  eof :: ∀ r. Boolean -> (_ -> Boolean) -> ArgParser r (List KeyValue)
  eof allowUnknown isKnownToken = do
    input <- getInput
    case input of
      Nil -> pure Nil
      tok:toks -> do
        let kToks = (tok:|toks) <#> \pTok -> if isKnownToken $ Tok.getToken pTok
                                                  then known pTok
                                                  else unknown pTok

        -- deal with unkown options matched after the pattern has ended.
        if allowUnknown
          then
            let ks = filter isKnown (NE.toList kToks)
                uks = filter isUnknown (NE.toList kToks)
             in case ks of
                  Nil ->
                    -- check the list of unknown tokens for literals which we
                    -- consider positional and therefore reject
                    let ukPs = filter (isPosTok <<< unIsKnown) uks
                     in case ukPs of
                          Nil -> Parser.return $ uks <#> unIsKnown >>>
                            \(pTok@(PositionedToken tok _ _)) ->
                              case tok of
                                Tok.EOA xs ->
                                  let v = ArrayValue $ A.fromFoldable xs
                                      rv = RichValue.from Origin.Argv v
                                    in _EOA /\ rv
                                _ ->
                                  let v = StringValue $ Tok.getSource pTok
                                      rv = RichValue.from Origin.Argv v
                                    in _UNKNOWN_ARG /\ rv
                          u:us -> fail' $ unexpectedInputError (u:|us)
                  k:ks -> fail' $ unexpectedInputError (k:|ks)
          else fail' $ unexpectedInputError kToks
    where isPosTok (PositionedToken tok _ _) = case tok of
                                                  Tok.Lit _ -> true
                                                  _ -> false

  emptyBranch :: _ -> Maybe _
  emptyBranch false = Nothing
  emptyBranch true = Just $ (Elem $ Arg.getArg _UNKNOWN_ARG)
                            :| (Elem $ Arg.getArg _EOA)
                                : Nil

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
  Remove singleton groups and single branch groups (where possible).
-}
simplifyLayout (ChoicePattern o r f (xs:Nil)) =
  let ys = concat $ simplifyLayout <$> xs
      t = all case _ of
            LeafPattern o r f x -> o || Arg.hasFallback x
            ChoicePattern o r f xs -> o || all id (t <$> xs)
   in if t ys
        then xs
        else singleton $ ChoicePattern o r f $ singleton ys
simplifyLayout (ChoicePattern o r f xs) = singleton $
  ChoicePattern o r f $ concat <<< (simplifyLayout <$> _) <$> xs
simplifyLayout p = singleton p

{-
  Convert a list of patterns containing solved layout arguments into a list of
  patterns containing pre-cached `Arg`s.
-}
toArgLeafs
  :: ∀ r
   . Options r
  -> Env
  -> List Description
  -> List (Pattern SolvedLayoutArg)
  -> List (Pattern Arg)
toArgLeafs options env descriptions xs = evalState (for xs go) 0
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
        canTerm = case x of
          Solved.Option a _ _ ->
            let aliases = case mDesc of
                  Just (OptionDescription as _ _ _ _) -> NE.toList as
                  _ -> singleton a
             in any (_ `elem` options.stopAt) $ aliases <#> case _ of
                      OA.Short s -> "-"  <> String.singleton s
                      OA.Long n  -> "--" <> n
          Solved.Positional _ r -> r && options.optionsFirst
          Solved.EOA -> true
          _ -> false

     in Arg id x (toArgKey x) canTerm mDesc fallback

{-
  Convert a layout into a "pattern" for the pattern parser to consume
-}
layoutToPattern
  :: Boolean -- are flags considered required?
  -> Boolean -- can options always repeat?
  -> SolvedLayout
  -> Pattern SolvedLayoutArg

layoutToPattern reqFlags repOpts (Elem x) = case x of
  Solved.Command    n r -> LeafPattern false r true x
  Solved.Positional n r -> LeafPattern false r true x
  Solved.Option  a Nothing r ->                        LeafPattern (not reqFlags) (r || repOpts) false x
  Solved.Option  a (Just (OptionArgument _ true)) r -> LeafPattern (not reqFlags) (r || repOpts) false x
  Solved.Option  a mA r -> LeafPattern false (r || repOpts) false x
  Solved.EOA            -> LeafPattern false false false x
  Solved.Stdin          -> LeafPattern false false false x

layoutToPattern reqFlags repOpts (Group o r xs) =
  let xs' = NE.toList do
              ((layoutToPattern reqFlags repOpts <$> _) <<< NE.toList) <$> do
                xs
      fix = any (any isFixed) xs'
   in ChoicePattern o r fix xs'
