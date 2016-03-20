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
import Control.Monad.State (State(), evalState)
import Control.Apply ((*>), (<*))
import Data.Function (on)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe, maybe')
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

import Language.Docopt.ParserGen.Token
import Language.Docopt.ParserGen.ValueMapping
import Language.Docopt.Parser.Base (alphaNum, space, getInput, debug)

type Parser a = P.ParserT (List Token) (Reader D.Env) a

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
token test = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case test tok of
        Just a ->
          let nextpos = pos -- neglect pos (for now)
          in
            { consumed: true
            , input:    xs
            , result:   Right a
            , position: nextpos }
        -- XXX: Fix this error message, it makes no sense!
        Nothing -> P.parseFailed toks pos "a better error message!"
    _ -> P.parseFailed toks pos "expected token, met EOF"

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
    go (Lit v) = Just (D.StringValue v)
    go _       = Nothing

type HasConsumedArg = Boolean
data OptParse = OptParse D.Value (Maybe Token) HasConsumedArg

longOption :: O.Name -> (Maybe O.Argument) -> Parser D.Value
longOption n a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case go tok (head xs) of
        Left e -> P.parseFailed toks pos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    (maybe empty singleton newtok) ++
                      (if hasConsumedArg then (LU.tail xs) else xs)
          , result:   Right v
          , position: pos -- ignore pos (for now)
          }
    _ -> P.parseFailed toks pos "expected token, met EOF"

  where
    takesArg = isJust a

    -- case 1:
    -- The name is an exact match
    go (LOpt n' v) atok | takesArg && (n' == n)
      = case v of
          Just s ->
            return $ OptParse (D.StringValue s) Nothing false
          _  -> return case atok of
            Just (Lit s) -> OptParse (D.StringValue s)  Nothing true
            _            -> OptParse (D.BoolValue true) Nothing false

    -- case 2:
    -- The name is an exact match and takes no argument
    go (LOpt n' _) _ | (not takesArg) && (n' == n)
      = return $ OptParse (D.BoolValue true) Nothing false

    -- case 3:
    -- The name is a substring of the input and no explicit argument has been
    -- provdided.
    go (LOpt n' Nothing) atok | takesArg
      = case stripPrefix n n' of
          Just s -> return $ OptParse (D.StringValue s) Nothing false
          _      -> Left "Invalid substring"

    go _ _ = Left "Invalid token"

shortOption :: Char -> (Maybe O.Argument) -> Parser D.Value
shortOption f a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case go tok (head xs) of
        Left e -> P.parseFailed toks pos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    (maybe empty singleton newtok) ++
                      (if hasConsumedArg then (LU.tail xs) else xs)
          , result:   Right v
          , position: pos -- ignore pos
          }
    _ -> P.parseFailed toks pos "expected token, met EOF"

  where
    takesArg = isJust a

    -- case 1:
    -- The leading flag matches, there are no stacked options, and an explicit
    -- argument may have been passed.
    go (SOpt f' xs v) atok | (f' == f) && takesArg && (A.length xs == 0)
      = case v of
          Just val -> return $ OptParse (D.StringValue val) Nothing false
          _  -> return case atok of
            Just (Lit s) -> OptParse (D.StringValue s) Nothing true
            _ -> OptParse (D.BoolValue true)
                          Nothing
                          false

    -- case 2:
    -- The leading flag matches, there are stacked options, no explicit
    -- argument has been passed and the option takes an argument.
    go (SOpt f' xs v) _ | (f' == f) && takesArg && (A.length xs > 0)
      = do
        let a = fromCharArray xs ++ maybe "" id v
        return $ OptParse (D.StringValue a)
                          Nothing
                          false

    -- case 3:
    -- The leading flag matches, there are stacked options, the option takes
    -- no argument and an explicit argument has not been provided.
    go (SOpt f' xs v) _ | (f' == f) && (not takesArg) && (A.length xs > 0)
      = return $ OptParse (D.BoolValue true)
                          (Just $ SOpt (AU.head xs) (AU.tail xs) v)
                          false

    -- case 4:
    -- The leading flag matches, there are no stacked options and the option
    -- takes no argument - total consumption!
    go (SOpt f' xs _) _ | (f' == f) && (not takesArg) && (A.length xs == 0)
      = return $ OptParse (D.BoolValue true)
                          Nothing
                          false

    go a b = Left $ "Invalid token " ++ show a ++ " (input: " ++ show b ++ ")"

eof :: Parser Unit
eof = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return $ case s of
    Nil -> { consumed: false, input: s, result: Right unit, position: pos }
    _   -> P.parseFailed s pos $
              "Trailing input: "
            ++ (intercalate ", " $ prettyPrintToken <$> s)

-- | Generate a parser for a single program usage.
genUsageParser :: D.Usage -> Parser (Tuple D.Branch (List ValueMapping))
genUsageParser (D.Usage xs) = genBranchesParser xs <* eof

-- | Fold over a list of parsers, applying each in turn onto the result of the
-- | previous, keeping both errors and results. Each application of a branches
-- | parser must set out from the same parser state.

-- | Generate a parser that selects the best branch it parses and
-- | fails if no branch was parsed.

genBranchesParser :: List D.Branch
                  -> Parser (Tuple D.Branch (List ValueMapping))
genBranchesParser xs = P.ParserT \(s@(P.PState { input: i, position: pos })) -> do
  env :: D.Env <- ask
  let ps = xs <#> \x -> rmapScoreResult (Tuple x) <$> genBranchParser x
      rs = runReader (collect s ps) env

  -- Weed out successful parses that have a zero-scores, from successful
  -- parses that have a non-zero score. Zero-scores are treated as errors
  -- due to ambiguity if no none-zero score was attained. Should there be
  -- only a single zero-score, however, it can walk away the victor.
  -- XXX: Should the ambiguity rules be extended for any score? I.e. if
  --      there's a total tie across all scores.
  (Tuple zs as) <- return $
    mpartition ((== 0) <<< _.score <<< unScoredResult <<< _.result)
             $ mrights rs

  return $ maybe'
    (\_ ->
      maybe'
        (\_ ->
          -- XXX: The following needs review. Is this safe? correct?
          let h = LU.head zs
            in h { result = Right <<< _.result <<< unScoredResult $ h.result }
        )
        (\e -> e { result = Left e.result })
        (head $ mlefts rs)
    )
    (\r -> r { result = Right <<< _.result <<< unScoredResult $ r.result })
    (maximumBy (compare `on` _.result) as)
  where
    collect s ps = ps `flip traverse` \p -> P.unParserT p s >>= \o ->
                    return $ either
                      (\e -> Left  { input:    o.input
                                   , consumed: o.consumed
                                   , position: o.position
                                   , result:   e
                                   })
                      (\r -> Right { input:    o.input
                                   , consumed: o.consumed
                                   , position: o.position
                                   , result:   r
                                   })
                      o.result



-- | Generate a parser for a single usage branch
genBranchParser :: D.Branch -> Parser (ScoredResult (List ValueMapping))
genBranchParser (D.Branch xs) = do
  either
    (\_   -> P.fail "Failed to generate parser")
    (\acc -> case acc of
              Free p       -> p
              Pending p xs -> do
                r  <- p
                rs <- genExhaustiveParser xs
                return $ r ++ rs
    )
    (foldM step (Free $ pure mempty) xs)
  where

    -- Given a list of arguments, try parse them all in any order.
    -- The only requirement is that all input is consumed in the end.
    genExhaustiveParser :: List D.Argument
                        -> Parser (ScoredResult (List ValueMapping))
    genExhaustiveParser Nil = return mempty
    genExhaustiveParser ps  = do
      draw ps (length ps)
      where
        -- iterate over `ps` until a match `x` is found, then, recursively
        -- apply `draw` until the parser fails, with a modified `ps`.
        draw :: List D.Argument -- ^ the arguments to parse
             -> Int             -- ^ the number of options left to parse
             -> Parser (ScoredResult (List ValueMapping))

        draw pss@(Cons p ps') n | n >= 0 = (do
          r <- unScoredResult <$> genParser p

          -- verify the arguments for parsed set of options
          -- when an option takes anything but a bool value, i.e. it is not
          -- a flag, an explicit argument *must* be provided.
          let ys = fst <$> flip filter r.result
                    (\(Tuple a v) -> (D.takesArgument a)
                                  && (not $ D.isFlag a)
                                  && (D.isBoolValue v))

          if (length ys == 0)
            then return unit
            else P.fail $ "Missing required arguments for "
                        ++ intercalate ", " (D.prettyPrintArg <$> ys)

          r' <- unScoredResult <$> do
                  if D.isRepeatable p
                        then draw pss (length pss)
                        else draw ps' (length ps')

          return $ ScoredResult r ++ ScoredResult r'
        ) <|> (defer \_ -> draw (ps' ++ singleton p) (n - 1))

        draw ps' n | (length ps' > 0) && (n < 0) = do
          env <- lift ask
          let missing = filter (\o -> not $ D.isRepeatable  o
                                         || D.hasDefault    o
                                         || D.isFlag        o
                                         || D.hasEnvBacking o env
                               )
                               $ reverse ps'
          if (length missing > 0)
            then P.fail $
              "Missing required options: "
                ++ intercalate ", " (D.prettyPrintArg <$> missing)
            else return mempty

        draw _ _ = return mempty

    step :: Acc (ScoredResult (List ValueMapping))
         -> D.Argument
         -> Either P.ParseError (Acc (ScoredResult (List ValueMapping)))

    -- Options always transition to the `Pending state`
    step (Free p) x@(D.Option _)
      = return $ Pending p (singleton x)

    step (Free p) x@(D.Group _ bs _) | isFree x
      = return $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      r  <- p
      rs <- genParser x
      return $ r ++ rs

    -- Options always keep accumulating
    step (Pending p xs) x@(D.Option _) = Right $ Pending p (x:xs)

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        r   <- p
        rs  <- genExhaustiveParser xs
        rss <- genParser y
        return $ r ++ rs ++ rss

    -- Parser generator for a single `Argument`
    genParser :: D.Argument
              -> Parser (ScoredResult (List ValueMapping))

    -- Generate a parser for a `Command` argument
    genParser x@(D.Command n) = (do
      scoreFromList <<< singleton <<< Tuple x <$> do
        command n
      ) P.<?> "command: " ++ (show $ D.prettyPrintArg x)

    -- Generate a parser for a `EOA` argument
    genParser x@(D.EOA) = (do
      scoreFromList <<< singleton <<< Tuple x <$> do
        eoa <|> (return $ D.ArrayValue []) -- XXX: Fix type
      ) P.<?> "end of arguments: \"--\""

    -- Generate a parser for a `Stdin` argument
    genParser x@(D.Stdin) = (do
      scoreFromList <<< singleton <<< Tuple x <$> do
        -- stdin always succeeds, as it is not actually an argument on argv.
        -- XXX: Should docopt check `process.stdin.isTTY` at this stage, or
        --      even at all?
        return (D.BoolValue true)
      ) P.<?> "stdin: \"-\""

    -- Generate a parser for a `Positional` argument
    genParser x@(D.Positional n r) = (do
      scoreFromList <$> do
        if r then (some go) else (singleton <$> go)
        ) P.<?> "positional argument: " ++ (show $ D.prettyPrintArg x)
        where go = Tuple x <$> (positional n)

    -- Generate a parser for a `Option` argument
    genParser x@(D.Option (O.Option o)) = (do
      scoreFromList <$> do
        if o.repeatable then (some go) else (singleton <$> go)
        ) P.<?> "option: " ++ (show $ D.prettyPrintArg x)
        where
          go = do
            P.choice $ P.try <$> [
              Tuple x <$> (mkLoptParser o.name o.arg)
            , Tuple x <$> (mkSoptParser o.flag o.arg)
            ]

          mkLoptParser (Just n) a = longOption n a
          mkLoptParser Nothing _  = P.fail "no long name"

          mkSoptParser (Just f) a = shortOption f a
          mkSoptParser Nothing _  = P.fail "no flag"

    -- Generate a parser for an argument `Group`
    -- The total score a group is the sum of all scores inside of it.
    genParser x@(D.Group optional bs repeated) = do
      vs <- concat <$>
          let mod    = if optional then P.try >>> P.option mempty else \p -> p
              parser = if repeated then many go else singleton <$> go
          in mod parser
      return $ scoreFromList vs
      where go = if length bs > 0
                    then snd <$> genBranchesParser bs
                    else return mempty

    isFree :: D.Argument -> Boolean
    isFree (D.Option _)     = true
    isFree (D.Group _ bs _) = all (\(D.Branch b) -> all isFree b) bs
    isFree _                = false
