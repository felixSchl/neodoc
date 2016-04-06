-- | Input Parser Generator for Docopt
-- |
-- | > Given a un-ambigious specification as input, generate a parser that can
-- | > be applied to user input.
-- |
-- | ===

module Language.Docopt.ParserGen.Parser (
    genUsageParser
  , Parser()
  ) where

import Prelude
import Control.Plus (class Plus, empty)
import Debug.Trace
import Data.Identity (Identity(), runIdentity)
import Control.Apply ((*>), (<*))
import Data.Function (on)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe, maybe', fromMaybe)
import Data.List (List(..), foldM, (:), singleton, some, toList, delete, length
                 , head, many, tail, fromList, filter, reverse, concat, catMaybes)
import Control.Alt ((<|>))
import Data.Traversable (class Traversable, traverse, for)
import Control.Lazy (defer)
import Data.Foldable (class Foldable, maximum, maximumBy, foldl, intercalate, for_, all)
import Data.String (fromCharArray, stripPrefix)
import Data.Bifunctor (class Bifunctor, rmap)
import Data.List as L
import Data.List.Unsafe as LU
import Data.Array as A
import Data.Array.Unsafe as AU
import Data.Array (uncons)
import Data.Monoid (Monoid)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Monoid (mempty)
import Data.Map (Map())
import Control.Monad.Reader (Reader(), ask, runReader)
import Control.Monad.Reader.Trans (ReaderT(), runReaderT)
import Control.Monad.State (State(), runState, evalState, execState)
import Control.Monad.State as State
import Data.Map as Map
import Data.StrMap (StrMap())
import Control.Monad.Trans (lift)
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Control.Bind ((=<<))

import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P

import Language.Docopt.Env      as Env
import Language.Docopt.Errors   as D
import Language.Docopt.Value    as D
import Language.Docopt.Argument as D
import Language.Docopt.Usage    as D
import Language.Docopt.Env      as D
import Language.Docopt.Option   as O
import Language.Docopt.Value    as Value

import Language.Docopt.ParserGen.Token
import Language.Docopt.ParserGen.Token as Token
import Language.Docopt.ParserGen.ValueMapping
import Language.Docopt.Parser.Base (alphaNum, space, getInput, debug)

-- |
-- | Unfortunately, the State Monad is needed because we try matching all
-- | program branches and must select the best fit.
-- |
type Parser a = P.ParserT (List PositionedToken)
                          (ReaderT D.Env (State Int))
                          a

newtype ScoredResult a = ScoredResult { score :: Int, result :: a }

unScoredResult :: forall a. ScoredResult a -> { score :: Int, result :: a }
unScoredResult (ScoredResult r) = r

instance semigroupScoredResult :: (Semigroup a) => Semigroup (ScoredResult a)
  where append (ScoredResult s) (ScoredResult s')
          = ScoredResult { score:  s.score  + s'.score
                         , result: s.result ++ s'.result }

instance monoidScoredResult :: (Monoid a) => Monoid (ScoredResult a)
  where mempty = ScoredResult { score: 0, result: mempty }

instance showScoredResult :: (Show a) => Show (ScoredResult a)
  where show (ScoredResult { score, result })
          = "ScoredResult " ++ show score ++ ": " ++ show result

instance ordScoredResult :: Ord (ScoredResult a)
  where compare = compare `on` (_.score <<< unScoredResult)

instance eqScoredResult :: Eq (ScoredResult a)
  where eq = eq `on` (_.score <<< unScoredResult)

scoreFromList :: forall a. List a -> ScoredResult (List a)
scoreFromList xs = ScoredResult { score: length xs, result: xs }

rmapScoreResult :: forall a b. (a -> b) -> ScoredResult a -> ScoredResult b
rmapScoreResult f (ScoredResult (x@{ result })) = ScoredResult $ x { result = f result }

--------------------------------------------------------------------------------
-- Input Token Parser ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test the token at the head of the stream
token :: forall a. (Token -> Maybe a) -> Parser a
token test = P.ParserT $ \(P.PState { input: toks, position: ppos }) ->
  return $ case toks of
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
    _ -> P.parseFailed toks ppos "expected token, met EOF"

data Acc a
  = Free (Parser a)
  | Pending (Parser a) (List D.Argument)

eoa :: Parser D.Value
eoa = token go P.<?> "--"
  where
    go (EOA xs) = Just (D.ArrayValue (fromList xs))
    go _        = Nothing

command :: String -> Parser D.Value
command n = token go P.<?> "command " ++ show n
  where
    go (Lit s) | s == n = Just (D.BoolValue true)
    go _                = Nothing

positional :: String -> Parser D.Value
positional n = token go P.<?> "positional argument " ++ show n
  where
    go (Lit v) = Just (Value.read v)
    go _       = Nothing

dash :: Parser D.Value
dash = token go P.<?> "stdin flag"
  where
    go (Lit v) | v == "-" = Just (D.BoolValue true)
    go _                  = Nothing

type HasConsumedArg = Boolean
data OptParse = OptParse D.Value (Maybe Token) HasConsumedArg

longOption :: O.Name -> (Maybe O.Argument) -> Parser D.Value
longOption n a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons (PositionedToken { token: tok, sourcePos: npos, source: s }) xs ->
      case go tok (_.token <<< unPositionedToken <$> head xs) of
        Left e -> P.parseFailed toks npos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    let pushed = maybe empty
                                         (\v -> singleton $ PositionedToken {
                                                  token:     v
                                                , sourcePos: pos
                                                , source:    s
                                                }
                                          )
                                          newtok
                          rest   = if hasConsumedArg then LU.tail xs else xs
                       in pushed ++ rest
          , result:   Right v
          , position: maybe pos
                            (_.sourcePos <<< unPositionedToken)
                            (head xs)
          }
    _ -> P.parseFailed toks pos "expected token, met EOF"

  where
    isFlag = isNothing a

    -- case 1:
    -- The name is an exact match
    go (LOpt n' v) atok | (not isFlag) && (n' == n)
      = case v of
          Just s ->
            return $ OptParse (Value.read s) Nothing false
          _  -> return case atok of
            Just (Lit s) -> OptParse (Value.read s)  Nothing true
            _            -> OptParse (D.BoolValue true) Nothing false

    -- case 2:
    -- The name is an exact match and takes no argument
    go (LOpt n' v) _ | isFlag && (n' == n)
      = case v of
             Just _  -> Left "Option takes no argument"
             Nothing -> return $ OptParse (D.BoolValue true) Nothing false

    -- case 3:
    -- The name is a substring of the input and no explicit argument has been
    -- provdided.
    go (LOpt n' Nothing) _ | not isFlag
      = case stripPrefix n n' of
          Just s -> return $ OptParse (Value.read s) Nothing false
          _      -> Left "Invalid substring"

    go a b = Left $ "Invalid token" ++ show a ++ " (input: " ++ show b ++ ")"

shortOption :: Char -> (Maybe O.Argument) -> Parser D.Value
shortOption f a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons (PositionedToken { token: tok, source: s }) xs ->
      case go tok (_.token <<< unPositionedToken <$> head xs) of
        Left e -> P.parseFailed toks pos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    let pushed = maybe empty
                                         (\v -> singleton $ PositionedToken {
                                                  token:     v
                                                , sourcePos: pos
                                                , source:    s
                                                }
                                          )
                                          newtok
                          rest   = if hasConsumedArg then LU.tail xs else xs
                       in pushed ++ rest
          , result:   Right v
          , position: maybe pos
                            (_.sourcePos <<< unPositionedToken)
                            (head xs)
          }
    _ -> P.parseFailed toks pos "expected token, met EOF"

  where
    isFlag = isNothing a

    -- case 1:
    -- The leading flag matches, there are no stacked options, and an explicit
    -- argument may have been passed.
    go (SOpt f' xs v) atok | (f' == f) && (not isFlag) && (A.length xs == 0)
      = case v of
          Just val -> return $ OptParse (D.StringValue val) Nothing false
          _  -> return case atok of
            Just (Lit s) -> OptParse (Value.read s) Nothing true
            _ -> OptParse (D.BoolValue true)
                          Nothing
                          false

    -- case 2:
    -- The leading flag matches, there are stacked options, no explicit
    -- argument has been passed and the option takes an argument.
    go (SOpt f' xs v) _ | (f' == f) && (not isFlag) && (A.length xs > 0)
      = do
        let a = fromCharArray xs ++ maybe "" id v
        return $ OptParse (Value.read a)
                          Nothing
                          false

    -- case 3:
    -- The leading flag matches, there are stacked options, the option takes
    -- no argument and an explicit argument has not been provided.
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (A.length xs > 0)
      = return $ OptParse (D.BoolValue true)
                          (Just $ SOpt (AU.head xs) (AU.tail xs) v)
                          false

    -- case 4:
    -- The leading flag matches, there are no stacked options and the option
    -- takes no argument - total consumption!
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (A.length xs == 0)
      = case v of
              Just _  -> Left "Option takes no argument"
              Nothing -> return $ OptParse (D.BoolValue true)
                                            Nothing
                                            false

    go a b = Left $ "Invalid token" ++ show a ++ " (input: " ++ show b ++ ")"

eof :: Parser Unit
eof = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return $ case s of
    Nil -> { consumed: false, input: s, result: Right unit, position: pos }
    _   -> P.parseFailed s pos $
              "Trailing input: "
            ++ (intercalate ", "
                  $ prettyPrintToken <<< _.token <<< unPositionedToken <$> s)

-- | Generate a parser for a single program usage.
genUsageParser :: List D.Usage -- ^ The list of usage specs
               -> Boolean      -- ^ Enable "options-first"
               -> Parser (Tuple D.Branch (List ValueMapping))
genUsageParser xs optsFirst = do
  _.result <<< unScoredResult
    <$> genBranchesParser (concat $ D.runUsage <$> xs)
                          true
                          optsFirst
                          true

-- | Generate a parser that selects the best branch it parses and
-- | fails if no branch was parsed.
genBranchesParser :: List D.Branch -- ^ The branches to test
                  -> Boolean       -- ^ Expect EOF after each branch
                  -> Boolean       -- ^ Enable "options-first"
                  -> Boolean       -- ^ Can we skip input via fallbacks?
                  -> Parser (ScoredResult (Tuple D.Branch (List ValueMapping)))
genBranchesParser xs term optsFirst canSkip
  = P.ParserT \(s@(P.PState { input: i, position: pos })) -> do
    env :: D.Env <- ask
    let ps = xs <#> \x -> rmapScoreResult (Tuple x) <$> do
                            genBranchParser x optsFirst canSkip
                            <* if term then eof else return unit
        rs = evalState (runReaderT (collect s ps) env) 0
    return $ maybe'
      (\_ ->
        maybe'
          (\_ -> P.parseFailed Nil P.initialPos
                  "The impossible happened. Failure without error")
          (\e -> e { result = Left e.result.error })
          (maximumBy (compare `on` (_.depth <<< _.result)) $ mlefts rs)
      )
      (\r -> r { result = Right r.result.value })
      (maximumBy (compare `on` (_.depth <<< _.result)) $ mrights rs)

  where
    collect s ps = ps `flip traverse` \p -> P.unParserT p s >>= \o -> do
                    depth :: Int <- lift State.get
                    return $ either
                      (\e -> Left  { input:    o.input
                                   , consumed: o.consumed
                                   , position: o.position
                                   , result:   { error: e, depth: depth }
                                   })
                      (\r -> Right { input:    o.input
                                   , consumed: o.consumed
                                   , position: o.position
                                   , result:   { value: r, depth: depth }
                                   })
                      o.result

-- | Generate a parser for a single usage branch
genBranchParser :: D.Branch  -- ^ The usage branch
                -> Boolean   -- ^ Enable "options-first"
                -> Boolean   -- ^ Can we skip input via fallbacks?
                -> Parser (ScoredResult (List ValueMapping))
genBranchParser (D.Branch xs) optsFirst canSkip = do
  lift (State.put 0) -- reset the depth counter
  either
    (\_   -> P.fail "Failed to generate parser")
    (\acc -> case acc of
              Free p       -> p
              Pending p xs -> do
                r  <- p
                rs <- genExhaustiveParser (reverse xs) canSkip
                return $ r ++ rs
    )
    (foldM step (Free $ pure mempty) xs)
  where

    -- Given a list of arguments, try parse them all in any order.
    -- The only requirement is that all input is consumed in the end.
    genExhaustiveParser :: List D.Argument -- ^ The free arguments
                        -> Boolean         -- ^ Can we skip input via fallbacks?
                        -> Parser (ScoredResult (List ValueMapping))
    genExhaustiveParser Nil canSkip = return mempty
    genExhaustiveParser ps  canSkip = do
      draw ps (length ps)
      where
        -- iterate over `ps` until a match `x` is found, then, recursively
        -- apply `draw` until the parser fails, with a modified `ps`.
        draw :: List D.Argument -- ^ the arguments to parse
             -> Int             -- ^ the number of options left to parse
             -> Parser (ScoredResult (List ValueMapping))

        draw pss@(Cons p ps') n | n >= 0 = (do
          -- Generate the parser for the argument `p`. For groups, temporarily
          -- set the required flag to "true", such that it will fail and we have
          -- a chance to retry as part of the exhaustive parsing mechanism
          r <- unScoredResult <$> (P.try $ genParser (D.setRequired p true)
                                                     (not $ n > 0))

          -- verify the arguments for parsed set of options
          -- when an option takes anything but a bool value, i.e. it is not
          -- a flag, an explicit argument *must* be provided.
          let missing = fst <$> flip filter r.result
                    (\(Tuple a v) -> (fromMaybe false do
                                        arg <- O.runArgument <$> do
                                                  D.getArgument a
                                        return $ not arg.optional
                                     )
                                  && (not $ D.isFlag a)
                                  && (D.isBoolValue v))

          if (length missing == 0)
            then return unit
            else P.fail $ "Missing required arguments for "
                        ++ intercalate ", " (D.prettyPrintArgNaked <$> missing)

          r' <- unScoredResult <$> do
                  if D.isRepeatable p
                        then draw pss (length pss)
                        else draw ps' (length ps')

          return $ ScoredResult r ++ ScoredResult r'
        ) <|> (defer \_ -> draw (ps' ++ singleton p) (n - 1))

        draw ps' n | (length ps' > 0) && (n < 0) = do
          env :: StrMap String <- lift ask

          let missing = filter (\o -> not $ (canSkip) && isSkippable env o) ps'

          if (length missing > 0)
            then P.fail $
              "Missing required options: "
                ++ intercalate ", " (D.prettyPrintArgNaked <$> missing)
            else return mempty

          where
            isSkippable env (D.Group o bs _)
              = o || (all (all (isSkippable env) <<< D.runBranch) bs)
            isSkippable env o
              =  (D.isRepeatable  o)
              || (D.hasDefault    o)
              || (maybe true id do
                  arg <- O.runArgument <$> do
                            D.getArgument o
                  return arg.optional
                )
              || (D.hasEnvBacking o env)

        draw _ _ = return mempty

    step :: Acc (ScoredResult (List ValueMapping))
         -> D.Argument
         -> Either P.ParseError (Acc (ScoredResult (List ValueMapping)))

    -- Options always transition to the `Pending state`
    step (Free p) x@(D.Option _)
      = return $ Pending p (singleton x)

    -- "Free" groups always transition to the `Pending state`
    step (Free p) x@(D.Group o (Cons (D.Branch b) Nil) r) | isFree x
      = let ys = concat $ go o r <$> b
         in return $ Pending p ys
      where go o r (D.Group o' (Cons (D.Branch b') Nil) r') =
              singleton $ D.Group o (singleton $ D.Branch $ concat $ go o' r' <$> b') r
            go o r x = singleton $ D.Group o (singleton $ D.Branch $ singleton x) r

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      r  <- p
      rs <- genParser x false
      return $ r ++ rs

    -- Options always keep accumulating
    step (Pending p xs) x@(D.Option _)
      = return $ Pending p (x:xs)

    -- "Free" groups always keep accumulating
    step (Pending p xs) x@(D.Group o (Cons (D.Branch b) Nil) r) | isFree x
      = let ys = concat $ go o r <$> b
         in return $ Pending p (ys ++ xs)
      where go o r (D.Group o' (Cons (D.Branch b') Nil) r') =
              singleton $ D.Group o (singleton $ D.Branch $ concat $ go o' r' <$> b') r
            go o r x = singleton $ D.Group o (singleton $ D.Branch $ singleton x) r

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        r   <- p
        rs  <- genExhaustiveParser xs true
        rss <- genParser y false
        return $ r ++ rs ++ rss

    -- Terminate the parser at the given argument and collect all subsequent
    -- values int an array ("options-first")
    terminate arg = do
      input <- getInput
      let rest = Tuple arg <$> do
                  D.StringValue <<< Token.getSource <$> input
      P.ParserT \(P.PState { position: pos }) ->
        return {
          consumed: true
        , input:    Nil
        , result:   return $ ScoredResult {
                      score: 1
                    , result: rest
                    }
        , position: pos
        }

    -- Parser generator for a single `Argument`
    genParser :: D.Argument -- ^ The argument to generate a parser for
              -> Boolean    -- ^ Can we skip input via fallbacks?
              -> Parser (ScoredResult (List ValueMapping))

    -- Generate a parser for a `Command` argument
    genParser x@(D.Command n r) _ = (do
      scoreFromList <$> do
        if r then (some go) else (singleton <$> go)
      <* lift (State.modify (1+))
      ) P.<?> "command: " ++ (show $ D.prettyPrintArg x)
        where go = Tuple x <$> (command n)

    -- Generate a parser for a `EOA` argument
    genParser x@(D.EOA) _ = (do
      scoreFromList <<< singleton <<< Tuple x <$> do
        eoa <|> (return $ D.ArrayValue []) -- XXX: Fix type
      <* lift (State.modify (1+))
      ) P.<?> "end of arguments: \"--\""

    -- Generate a parser for a `Stdin` argument
    genParser x@(D.Stdin) _ = (do
      scoreFromList <<< singleton <<< Tuple x <$> do
        dash
        return (D.BoolValue true)
      <* lift (State.modify (1+))
      ) P.<?> "stdin: \"-\""

    genParser x@(D.Positional n r) _
      | r && optsFirst
      = terminate x

    -- Generate a parser for a `Positional` argument
    genParser x@(D.Positional n r) _ = (do
      scoreFromList <$> do
        if r then (some go) else (singleton <$> go)
      <* lift (State.modify (1+))
      ) P.<?> "positional argument: " ++ (show $ D.prettyPrintArg x)
        where go = Tuple x <$> (positional n)

    -- Generate a parser for a `Option` argument
    genParser x@(D.Option (O.Option o)) _ = (do
      scoreFromList <$> do
        if o.repeatable then (some go) else (singleton <$> go)
      <* lift (State.modify (1+))
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
            if isLopt
               then P.try $ Tuple x <$> mkLoptParser o.name o.arg
               else if isSopt
                then P.try $ Tuple x <$> mkSoptParser o.flag o.arg
                else P.fail "long or short option"

          isAnyLopt (LOpt _ _) = return true
          isAnyLopt _          = return false

          isAnySopt (SOpt _ _ _) = return true
          isAnySopt _            = return false

          mkLoptParser (Just n) a = longOption n a
          mkLoptParser Nothing _  = P.fail "long name"

          mkSoptParser (Just f) a = shortOption f a
          mkSoptParser Nothing _  = P.fail "flag"

    genParser x@(D.Group optional bs r) _
      | optional && optsFirst && (length bs == 1) &&
        all (\(D.Branch xs) ->
          case xs of
            Cons (D.Positional n r') Nil -> r' || r
            _                            -> false
        ) bs
      = terminate (LU.head (D.runBranch (LU.head bs)))

    -- Generate a parser for an argument `Group`
    -- The total score a group is the sum of all scores inside of it.
    genParser x@(D.Group optional bs repeated) canSkip = do
      vs <- concat <$>
          let mod    = if optional then P.try >>> P.option mempty else \p -> p
              parser = if repeated then goR else singleton <$> go
          in mod parser
      return $ scoreFromList vs
      where
        goR :: Parser (List (List ValueMapping))
        goR = do
          {score, result} <- unScoredResult
            <$> genBranchesParser bs
                                  false
                                  false
                                  -- always allow skipping for non-free groups.
                                  (not (isFree x) || canSkip)

          if score == 0
              then return $ singleton (snd result)
              else do
                xs <- goR <|> pure Nil
                return $ (snd result) : xs

        go :: Parser (List ValueMapping)
        go = if length bs == 0
                  then return mempty
                  else do
                    snd <<< _.result <<<  unScoredResult
                      <$> genBranchesParser bs
                                            false
                                            false
                                            -- always allow skipping for
                                            -- non-free groups.
                                            (not (isFree x) || canSkip)

    isFree :: D.Argument -> Boolean
    isFree (D.Option _)                                         = true
    isFree (D.Group _ (Cons (D.Branch b) Nil) _) | all isFree b = true
    isFree _                                                    = false
