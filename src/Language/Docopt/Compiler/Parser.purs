-- | Input Parser Generator for Docopt
-- |
-- | > Given a un-ambigious specification as input, generate a parser that can
-- | > be applied to user input.
-- |
-- | ===

module Language.Docopt.Compiler.Parser (
    genUsageParser
  , initialState
  , Parser ()
  , StateObj ()
  , ValueMapping ()
  , GenOptionsObj ()
  ) where

import Prelude
import Control.Monad.RWS (RWS(), evalRWS)
import Control.Plus (empty)
import Control.Bind ((=<<))
import Debug.Trace
import Control.Apply ((<*), (*>))
import Data.Function (on)
import Data.Bifunctor
import Data.Either (Either(..), either, isRight)
import Data.Maybe (Maybe(..), maybe, fromMaybe, maybe', isNothing)
import Data.List (List(..), foldM, reverse, singleton, concat, length, (:),
                  some, filter, head, fromList, sortBy, groupBy, last, null,
                  tail)
import Data.List.Lazy (take, repeat, fromList) as LL
import Control.Alt ((<|>))
import Data.Traversable (traverse)
import Control.Lazy (defer)
import Control.Monad (when, unless)
import Control.MonadPlus (guard)
import Data.Foldable (all, intercalate, maximumBy, sum, any, for_, foldl, elem)
import Data.String as String
import Data.String (fromChar, fromCharArray, stripPrefix)
import Data.List.Unsafe as LU
import Data.Array as A
import Data.Array.Unsafe as AU
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..), fst, snd)
import Control.Monad.Reader (ask)
import Control.Monad.State as State
import Data.StrMap (StrMap())
import Control.Monad.Trans (lift)
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Text.Parsing.Parser (PState(..), ParseError(..), ParserT(..), fail,
                            parseFailedFatal, parseFailed, unParserT, fatal) as P
import Text.Parsing.Parser.Combinators (option, try, lookAhead, (<?>)) as P
import Text.Parsing.Parser.Pos (Position, initialPos) as P

import Language.Docopt.Argument (Argument(..), Branch, isFree,
                                prettyPrintArg, prettyPrintArgNaked,
                                hasEnvBacking, getArgument, hasDefault,
                                isRepeatable, isFlag, setRequired,
                                OptionArgumentObj()
                                ) as D
import Language.Docopt.Usage (Usage, runUsage) as D
import Language.Docopt.Env (Env ())
import Language.Docopt.Env as Env
import Language.Docopt.Origin as Origin
import Language.Docopt.Origin (Origin())
import Language.Docopt.Value as Value
import Language.Docopt.Value (Value(..))
import Language.Docopt.RichValue (RichValue(..), unRichValue)
import Language.Docopt.RichValue (from, setValue, getOrigin) as RValue
import Language.Docopt.Parser.Base (getInput)
import Language.Docopt.Compiler.Token as Token
import Language.Docopt.Compiler.Token (PositionedToken(..), Token(..),
                                        unPositionedToken, prettyPrintToken)
import Data.String.Ext (startsWith)

-- | Toggle debugging on/off during development
debug :: Boolean
debug = false

-- | The output value mappings of arg -> val
type ValueMapping = Tuple D.Argument RichValue

-- | The stateful parser type
type StateObj = { depth :: Int }

-- | The CLI parser
type Parser a = P.ParserT (List PositionedToken)
                          (RWS Env Unit StateObj)
                          a

initialState :: StateObj
initialState = { depth: 0 }

modifyDepth :: (Int -> Int) -> Parser Unit
modifyDepth f = lift (State.modify \s -> s { depth = f s.depth })

-- | The options for generating a parser
type GenOptionsObj r = {
  optionsFirst :: Boolean
, stopAt       :: Array String
  | r
}

--------------------------------------------------------------------------------
-- Input Token Parser ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test the token at the head of the stream
token :: forall a. (Token -> Maybe a) -> Parser a
token test = P.ParserT $ \(P.PState { input: toks, position: ppos }) ->
  pure $ case toks of
    Cons (PositionedToken { token: tok }) xs ->
      case test tok of
        Just a ->
          let nextpos =
              case xs of
                Cons (PositionedToken { sourcePos: npos }) _ -> npos
                Nil -> ppos
          in
            { consumed: true
            , input: xs
            , result: Right a
            , position: nextpos }
        -- XXX: Fix this error message, it makes no sense!
        Nothing -> P.parseFailed toks ppos "a better error message!"
    _ -> P.parseFailed toks ppos "Expected token, met EOF"

data Acc a
  = Free (Parser a)
  | Pending (Parser a) (List D.Argument)

eoa :: Parser Value
eoa = token go P.<?> "--"
  where
    go (EOA xs) = Just (ArrayValue (fromList xs))
    go _        = Nothing

command :: String -> Parser Value
command n = token go P.<?> "command " <> show n
  where
    go (Lit s) | s == n = Just (BoolValue true)
    go _                = Nothing

positional :: String -> Parser Value
positional n = token go P.<?> "positional argument " <> show n
  where
    go (Lit v) = Just (Value.read v false)
    go _       = Nothing

stdin :: Parser Value
stdin = token go P.<?> "stdin flag"
  where
    go Stdin = Just (BoolValue true)
    go _     = Nothing

type HasConsumedArg = Boolean
data OptParse = OptParse Value (Maybe Token) HasConsumedArg

longOption :: String -> (Maybe D.OptionArgumentObj) -> Parser Value
longOption n a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  pure $ case toks of
    Cons (PositionedToken { token: tok, sourcePos: npos, source: s }) xs ->
      case go tok (_.token <<< unPositionedToken <$> head xs) of
        Left e -> P.parseFailed toks npos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    let pushed = maybe empty
                                         (\v' -> singleton $ PositionedToken {
                                                  token:     v'
                                                , sourcePos: pos
                                                , source:    s
                                                }
                                          )
                                          newtok
                          rest   = if hasConsumedArg then LU.tail xs else xs
                       in pushed <> rest
          , result:   Right v
          , position: maybe pos
                            (_.sourcePos <<< unPositionedToken)
                            (head xs)
          }
    _ -> P.parseFailed toks pos "Expected token, met EOF"

  where
    isFlag = isNothing a

    -- case 1:
    -- The name is an exact match
    go (LOpt n' v) atok | (not isFlag) && (n' == n)
      = case v of
          Just s ->
            pure $ OptParse (Value.read s false) Nothing false
          _  -> case atok of
            Just (Lit s) -> pure $ OptParse (Value.read s false)  Nothing true
            otherwise    ->
              if (fromMaybe true (_.optional <$> a))
                 then Right $ OptParse (BoolValue true) Nothing false
                 else Left  $ "Option requires argument: --" <> n'

    -- case 2:
    -- The name is an exact match and takes no argument
    go (LOpt n' v) _ | isFlag && (n' == n)
      = case v of
             Just _  -> Left $ "Option takes no argument: --" <> n'
             Nothing -> pure $ OptParse (BoolValue true) Nothing false

    -- case 3:
    -- The name is a substring of the input and no explicit argument has been
    -- provdided.
    go (LOpt n' Nothing) _ | not isFlag
      = case stripPrefix n n' of
          Just s -> pure $ OptParse (Value.read s false) Nothing false
          _      -> Left "Invalid substring"

    go a b = Left $ "Invalid token: " <> show a <> " (input: " <> show b <> ")"

shortOption :: Char -> (Maybe D.OptionArgumentObj) -> Parser Value
shortOption f a = P.ParserT $ \(P.PState { input: toks, position: pos }) -> do
  pure $ case toks of
    Cons (PositionedToken { token: tok, source: s }) xs ->
      case go tok (_.token <<< unPositionedToken <$> head xs) of
        Left e -> P.parseFailed toks pos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:
              let
                pushed = maybe empty
                                (\v' ->
                                  singleton
                                    $ PositionedToken
                                        { token:     v'
                                        , sourcePos: pos
                                        , source:    "-" <> String.drop 2 s
                                        })
                                newtok
                rest = if hasConsumedArg then LU.tail xs else xs
              in pushed <> rest
          , result:   Right v
          , position: maybe pos
                            (_.sourcePos <<< unPositionedToken)
                            (head xs)
          }
    _ -> P.parseFailed toks pos "Expected token, met EOF"

  where
    isFlag = isNothing a

    -- case 1:
    -- The leading flag matches, there are no stacked options, and an explicit
    -- argument may have been passed.
    go (SOpt f' xs v) atok | (f' == f) && (not isFlag) && (A.null xs)
      = case v of
          Just s    -> pure $ OptParse (Value.read s false) Nothing false
          otherwise -> case atok of
            Just (Lit s) -> pure $ OptParse (Value.read s false) Nothing true
            otherwise    ->
              if (fromMaybe true (_.optional <$> a))
                 then Right $ OptParse (BoolValue true) Nothing false
                 else  Left $ "Option requires argument: -" <> fromChar f'

    -- case 2:
    -- The leading flag matches, there are stacked options, a explicit
    -- argument may have been passed and the option takes an argument.
    go (SOpt f' xs v) _ | (f' == f) && (not isFlag) && (not $ A.null xs)
      = do
        let arg = fromCharArray xs <> maybe "" ("=" <> _) v
        pure $ OptParse (Value.read arg false)
                          Nothing
                          false

    -- case 3:
    -- The leading flag matches, there are stacked options, the option takes
    -- no argument and an explicit argument has not been provided.
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (not $ A.null xs)
      = pure $ OptParse (BoolValue true)
                          (Just $ SOpt (AU.head xs) (AU.tail xs) v)
                          false

    -- case 4:
    -- The leading flag matches, there are no stacked options and the option
    -- takes no argument - total consumption!
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (A.null xs)
      = case v of
              Just _  -> Left $ "Option takes no argument: -" <> fromChar f'
              Nothing -> pure $ OptParse (BoolValue true)
                                            Nothing
                                            false

    go a b = Left $ "Invalid token: " <> show a <> " (input: " <> show b <> ")"

eof :: Parser Unit
eof = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  pure $ case s of
    Nil -> { consumed: false, input: s, result: Right unit, position: pos }
    (Cons (PositionedToken {token: tok, source}) _) ->
      P.parseFailed s pos
        $ case tok of
              LOpt _ _   -> "Unmatched option: " <> source
              SOpt _ _ _ -> "Unmatched option: " <> source
              EOA _      -> "Unmatched option: --"
              Stdin      -> "Unmatched option: -"
              Lit _      -> "Unmatched command: " <> source


-- | Generate a parser for a single program usage.
genUsageParser :: forall r
                . List D.Usage    -- ^ The list of usage specs
               -> GenOptionsObj r -- ^ Generator options
               -> Parser (Tuple D.Branch (List ValueMapping))
genUsageParser xs genOpts = do
    genBranchesParser (concat $ D.runUsage <$> xs)
                      true
                      genOpts
                      true
                      0

-- | Generate a parser that selects the best branch it parses and
-- | fails if no branch was parsed.
genBranchesParser :: forall r
                   . List D.Branch   -- ^ The branches to test
                  -> Boolean         -- ^ Expect EOF after each branch
                  -> GenOptionsObj r -- ^ Generator options
                  -> Boolean         -- ^ Can we skip input via fallbacks?
                  -> Int             -- ^ The current recursive depth
                  -> Parser (Tuple D.Branch (List ValueMapping))
genBranchesParser xs term genOpts canSkip recDepth
  = P.ParserT \(s@(P.PState { input: i, position: pos })) -> do
    env   :: Env      <- ask
    state :: StateObj <- State.get

    let
      ps = xs <#> \x -> (Tuple x) <$> do
                          genBranchParser x genOpts canSkip recDepth
                            <* unless (not term) eof
      rs  = fst $ evalRWS (collect s ps) env initialState
      rs' = reverse rs

      -- Evaluate the winning candidates, if any.
      -- First, sort the positive results by their depth, then group consecutive
      -- elements, take the highest element and sort the inner values by their
      -- fallback score.
      winner =
          maximumBy (compare `on`
                      (((_.origin <<< unRichValue <<< snd) <$> _) <<< snd
                        <<< _.value <<< _.result)) =<< do
                          last $ groupBy (eq `on` (_.depth <<< _.result))
                                  (sortBy (compare `on` (_.depth <<< _.result))
                                          (mrights rs'))

      failures = mlefts rs

      -- Evaluate the losing candidates, if any.
      losers =
          last $ groupBy (eq `on` (_.depth <<< _.result))
                         (sortBy (compare `on` (_.depth <<< _.result))
                                 failures)

      fatal = last $ filter (\x ->
                let err = unParseError (x.result.error)
                 in err.fatal
                ) failures

    maybe
      (do
        maybe'
          (\_ ->
            fromMaybe
              (pure do
                P.parseFailed i pos
                    "The impossible happened. Failure without error")
              (do
                es <- losers
                pure case es of
                  Cons e es | null es || not (null i) -> do
                    pure $ e { result = Left e.result.error }
                  otherwise -> pure $ P.parseFailed i pos ""
              )
          )
          (\r -> pure $ r { result = Right r.result.value })
          winner
      )
      (\x ->
        let err = unParseError (x.result.error)
         in pure $ P.parseFailedFatal i pos err.message
      )
      fatal

  where
  fixMessage m = m
  collect s ps = ps `flip traverse` \p -> do
    o                 <- P.unParserT p s
    state :: StateObj <- State.get
    pure $ bimap
      (\e -> o { result = { error: e, depth: state.depth } })
      (\r -> o { result = { value: r, depth: state.depth } })
      o.result

-- | Generate a parser for a single usage branch
genBranchParser :: forall r
                 . D.Branch        -- ^ The branch to match
                -> GenOptionsObj r -- ^ Generator options
                -> Boolean         -- ^ Can we skip input via fallbacks?
                -> Int             -- ^ The current recursive depth
                -> Parser (List ValueMapping)
genBranchParser xs genOpts canSkip recDepth = do
  modifyDepth (const 0) -- reset the depth counter
  either
    (\_   -> P.fail "Failed to generate parser")
    (\acc -> case acc of
              Free p       -> p
              Pending p xs -> do
                r  <- p
                rs <- genExhaustiveParser (reverse xs) canSkip
                pure $ r <> rs
    )
    (foldM step (Free $ pure mempty) xs)
  where

    -- Given a list of arguments, try parse them all in any order.
    -- The only requirement is that all input is consumed in the end.
    genExhaustiveParser :: List D.Argument -- ^ The free arguments
                        -> Boolean         -- ^ Can we skip input via fallbacks?
                        -> Parser (List ValueMapping)
    genExhaustiveParser Nil canSkip = pure mempty
    genExhaustiveParser ps  canSkip = do
      when debug do
        pure unit -- prevent early execution of code below. PS bug?
        traceA $
          indentation <>
          "genExhaustiveParser: "
                <> (intercalate " " (D.prettyPrintArg <$> ps))
                <> " - canSkip: " <>  show canSkip
      draw ps (length ps) Nil

      where
        -- iterate over `ps` until a match `x` is found, then, recursively
        -- apply `draw` until the parser fails, with a modified `ps`.
        draw :: List D.Argument -- ^ the arguments to parse
             -> Int             -- ^ the number of options left to parse
             -> List D.Argument -- ^ the unique arguments parsed
             -> Parser (List ValueMapping)

        draw pss@(Cons p ps') n tot | n >= 0 = (do
          when debug do
            pure unit -- prevent early execution of code below. PS bug?
            i <- getInput
            traceA $
              indentation <>
              "draw: (" <> (D.prettyPrintArg p) <> ":"
                        <> (intercalate ":" (D.prettyPrintArg <$> ps'))
                        <>  ") - n: " <> show n
                        <> " from input: "
                        <> (intercalate " " (prettyPrintToken
                                              <<< _.token
                                              <<< unPositionedToken <$> i))

          -- Generate the parser for the argument `p`. For groups, temporarily
          -- set the required flag to "true", such that it will fail and we have
          -- a chance to retry as part of the exhaustive parsing mechanism
          r <- P.try $ genParser (D.setRequired p true) false

          r' <- P.try do
                  if (D.isRepeatable p &&
                      length (filter (snd >>> isFrom Origin.Argv) r) > 0)
                        then do
                          -- Make successive matches of this repeated
                          -- group optional.
                          draw ((D.setRequired p false):ps')
                                (length pss)
                                (p:tot)
                        else draw ps' (length ps') (p:tot)

          pure $ r <> r'
        ) <|> (defer \_ -> draw (ps' <> singleton p) (n - 1) tot)

        draw ps' n tot | (length ps' > 0) && (n < 0) = do
          env :: StrMap String <- lift ask

          i <- getInput
          let
            vs = ps' <#> \o ->
              maybe
                (Left o)
                (Right <<< Tuple o)
                do
                  v <- unRichValue <$> do
                    -- only allow "skipping" - that is auto-filling - values
                    -- if we are either explicitly allowed to do so (i.e.
                    -- the `canSkip` flag is set) or if we consumed all input
                    guard (null i || canSkip || (length ps' < length ps))
                    (getEnvValue env o <#> RValue.from Origin.Environment) <|>
                    (getDefaultValue o <#> RValue.from Origin.Default)     <|>
                    (getEmptyValue   o <#> RValue.from Origin.Empty)

                  pure $ RichValue v {
                    value = if D.isRepeatable o
                        then ArrayValue $ Value.intoArray v.value
                        else v.value
                  }

            missing   = filter (\o -> not ((null i || canSkip) &&
                                  isSkippable o)) (mlefts vs)
            fallbacks = mrights vs

          if canSkip
            then do
              xs <- genExhaustiveParser missing false
              pure $ fallbacks <> xs
            else
              if (length missing > 0)
                then P.fail $
                  "Expected option(s): "
                    <> intercalate ", " (D.prettyPrintArgNaked <$> missing)
                else pure fallbacks

          where
          isSkippable (D.Group grp)
              = grp.optional || (all (all isSkippable) grp.branches)
          isSkippable o = D.isRepeatable o && (any (_ == o) tot)

          getEnvValue :: Env -> D.Argument -> Maybe Value
          getEnvValue env (D.Option (o@{ env: Just k })) = do
            StringValue <$> Env.lookup k env
          getEnvValue _ _ = Nothing

          getDefaultValue :: D.Argument -> Maybe Value
          getDefaultValue (D.Option (o@{
              arg: Just { default: Just v }
            })) = pure if o.repeatable
                            then ArrayValue $ Value.intoArray v
                            else v
          getDefaultValue _ = Nothing

          getEmptyValue :: D.Argument -> Maybe Value
          getEmptyValue = go
            where
            go (D.Option (o@{ arg: Nothing }))
              = pure
                  $ if o.repeatable then ArrayValue []
                                    else BoolValue false
            go (D.Option (o@{ arg: Just { optional: true } }))
              = pure
                  $ if o.repeatable then ArrayValue []
                                    else BoolValue false
            go (D.Stdin)                           = pure $ BoolValue false
            go (D.EOA)                             = pure $ ArrayValue []
            go (D.Positional pos) | pos.repeatable = pure $ ArrayValue []
            go (D.Command cmd)    | cmd.repeatable = pure $ ArrayValue []
            go _                                   = Nothing

        draw _ _ _ = pure mempty

    step :: Acc (List ValueMapping)
         -> D.Argument
         -> Either P.ParseError (Acc (List ValueMapping))

    -- Options always transition to the `Pending state`
    step (Free p) x@(D.Option _)
      = pure $ Pending p (singleton x)

    -- "Free" groups always transition to the `Pending state`
    step (Free p) x@(D.Group _) | D.isFree x
      = pure $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      r  <- p
      rs <- genParser x false
      pure $ r <> rs

    -- Options always keep accumulating
    step (Pending p xs) x@(D.Option _)
      = pure $ Pending p (x:xs)

    -- "Free" groups always keep accumulating
    step (Pending p xs) x@(D.Group _) | D.isFree x
      = pure $ Pending p (x:xs)

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        r   <- p
        rs  <- genExhaustiveParser xs true
        rss <- genParser y true
        pure $ r <> rs <> rss

    -- Terminate the parser at the given argument and collect all subsequent
    -- values int an array ("options-first")
    terminate arg includeSelf = do
      input <- getInput
      let rest = Tuple arg <<< (RValue.from Origin.Argv) <$> do
                  StringValue <<< Token.getSource <$> do
                    if includeSelf
                       then input
                       else fromMaybe Nil (tail input)
      P.ParserT \(P.PState { position: pos }) ->
        pure {
          consumed: true
        , input:    Nil
        , result:   pure rest
        , position: pos
        }

    -- Parser generator for a single `Argument`
    genParser :: D.Argument -- ^ The argument to generate a parser for
              -> Boolean    -- ^ Can we skip input via fallbacks?
              -> Parser (List ValueMapping)

    -- Generate a parser for a `Command` argument
    genParser x@(D.Command cmd) _ = do
      debugParsing x
      i <- getInput
      (do
        if cmd.repeatable then (some go) else (singleton <$> go)
      ) <|> (P.fail $ "Expected " <> D.prettyPrintArg x <> butGot i)
        where go = do Tuple x <<< (RValue.from Origin.Argv) <$> (do
                        v <- command cmd.name
                        pure if cmd.repeatable
                                then ArrayValue $ Value.intoArray v
                                else v
                      )
                      <* modifyDepth (_ + 1)

    -- Generate a parser for a `EOA` argument
    genParser x@(D.EOA) _ = do
      debugParsing x
      singleton <<< Tuple x <<< (RValue.from Origin.Argv) <$> (do
        eoa <|> (pure $ ArrayValue []) -- XXX: Fix type
        <* modifyDepth (_ + 1)
      ) <|> P.fail "Expected \"--\""

    -- Generate a parser for a `Stdin` argument
    genParser x@(D.Stdin) _ = do
      debugParsing x
      singleton <<< Tuple x <<< (RValue.from Origin.Argv) <$> (do
        stdin
        <* modifyDepth (_ + 1)
      ) <|> P.fail "Expected \"-\""

    -- Terminate parsing at first positional argument
    genParser x@(D.Positional pos) _
      | pos.repeatable && genOpts.optionsFirst
      = terminate x true

    -- Generate a parser for a `Positional` argument
    genParser x@(D.Positional pos) _ = do
      debugParsing x
      i <- getInput
      (do
        if pos.repeatable then (some go) else (singleton <$> go)
      ) <|> P.fail ("Expected " <> D.prettyPrintArg x <> butGot i)
        where go = do Tuple x <<< (RValue.from Origin.Argv) <$> (do
                        v <- positional pos.name
                        pure if pos.repeatable
                                then ArrayValue $ Value.intoArray v
                                else v
                        )
                      <* modifyDepth (_ + 1)

    -- Terminate at singleton groups that house only positionals.
    genParser x@(D.Group grp) _
      | genOpts.optionsFirst && (length grp.branches == 1) &&
        all (\xs ->
          case xs of
            Cons (D.Positional pos) Nil -> pos.repeatable || grp.repeatable
            _                           -> false
        ) grp.branches
      = terminate (LU.head (LU.head grp.branches)) true

    -- Terminate at option if part of 'stopAt'
    genParser x@(D.Option o) _ |
      let names = A.catMaybes [ ("--" ++ _) <$> o.name
                              , ("-"  ++ _) <<< fromChar <$> o.flag
                              ]
       in any (_ `elem` genOpts.stopAt) names
      = terminate x false

    -- Generate a parser for a `Option` argument
    genParser x@(D.Option o) _ = (do
      debugParsing x
      do
        if o.repeatable
           then some          go
           else singleton <$> go
      <* modifyDepth (_ + 1)
      )
        where
          go = do
            -- Perform a pre-cursory test in order to capture relevant error
            -- messags which would otherwise be overriden (e.g. a meaningful
            -- error message when trying to match a LOpt against a LOpt would
            -- be overridden by a meaningless try to match a SOpt against a
            -- a LOpt).
            isLopt <- P.option false (P.lookAhead $ P.try $ token isAnyLopt)
            isSopt <- P.option false (P.lookAhead $ P.try $ token isAnySopt)
            P.ParserT \s -> do
              o <- P.unParserT (if isLopt
                then P.try do
                  Tuple x <<< (RValue.from Origin.Argv) <$> (do
                    v <- mkLoptParser o.name o.arg
                    pure if o.repeatable then ArrayValue $ Value.intoArray v
                                           else v
                  )
                else if isSopt
                  then P.try do
                    Tuple x <<< (RValue.from Origin.Argv) <$> (do
                      v <- mkSoptParser o.flag o.arg
                      pure if o.repeatable then ArrayValue $ Value.intoArray v
                                            else v
                    )
                  else P.fail "long or short option") s
              case o.result of
                  (Left e) -> do
                    let err = unParseError e
                    if ((startsWith
                            "Option takes no argument"
                            err.message)
                       || (startsWith
                            "Option requires argument"
                            err.message))
                      then do
                        pure $ o {
                          result = Left $ P.ParseError $ err {
                            fatal = true
                          }
                        }
                      else pure o
                  otherwise -> pure o

          isAnyLopt (LOpt _ _) = pure true
          isAnyLopt _          = pure false

          isAnySopt (SOpt _ _ _) = pure true
          isAnySopt _            = pure false

          mkLoptParser (Just n) a = longOption n a
          mkLoptParser Nothing _  = P.fail "long name"

          mkSoptParser (Just f) a = shortOption f a
          mkSoptParser Nothing _  = P.fail "flag"

    -- Generate a parser for an argument `Group`
    genParser x@(D.Group grp) canSkip = do
      debugParsing x
      if grp.optional
        then P.option mempty $ P.try go
        else go
      where
        go | length grp.branches == 0 = pure mempty
        go = do
          x <- step
          if grp.repeatable
            && length (filter (snd >>> isFrom Origin.Argv) x) > 0
              then do
                xs <- go <|> pure mempty
                pure $ x <> xs
              else pure x

        step = snd <$> do
                genBranchesParser grp.branches
                                  false
                                  genOpts
                                  -- always allow skipping for non-free groups.
                                  (not (D.isFree x) || canSkip)
                                  (recDepth + 1)

    butGot :: List PositionedToken -> String
    butGot (Cons (PositionedToken { source }) _) = ", but got " <> source
    butGot Nil                                   = ""

    isFrom :: Origin -> RichValue -> Boolean
    isFrom o rv = RValue.getOrigin rv == o

    debugParsing x = when debug do
      pure unit -- prevent early execution of code below. PS bug?
      traceA $
        indentation <>
        "Parsing: " <> D.prettyPrintArg x

    indentation :: String
    indentation = fromCharArray $ LL.fromList $ LL.take (recDepth * 2) $ LL.repeat ' '

unParseError :: P.ParseError -> { position :: P.Position, message :: String, fatal :: Boolean }
unParseError (P.ParseError e) = e
