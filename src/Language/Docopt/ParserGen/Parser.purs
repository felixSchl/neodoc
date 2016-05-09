-- | Input Parser Generator for Docopt
-- |
-- | > Given a un-ambigious specification as input, generate a parser that can
-- | > be applied to user input.
-- |
-- | ===

module Language.Docopt.ParserGen.Parser (
    genUsageParser
  , initialState
  , Parser ()
  , StateObj ()
  , RichValue (..)
  , RichValueObj (..)
  , ValueMapping ()
  , unRichValue
  , from
  ) where

import Prelude
import Control.Plus (empty)
import Control.Bind ((=<<))
import Debug.Trace
import Control.Apply ((<*), (*>))
import Data.Function (on)
import Data.Bifunctor
import Data.Either (Either(..), either, isRight)
import Data.Maybe (Maybe(..), maybe, fromMaybe, maybe', isNothing)
import Data.List (List(..), foldM, reverse, singleton, concat, length, (:),
                  some, filter, head, fromList, sortBy, groupBy, last, null)
import Control.Alt ((<|>))
import Data.Traversable (traverse)
import Control.Lazy (defer)
import Control.Monad (when, unless)
import Control.MonadPlus (guard)
import Data.Foldable (all, intercalate, maximumBy, sum, any, for_, foldl)
import Data.String as String
import Data.String (fromChar, fromCharArray, stripPrefix)
import Data.List.Unsafe as LU
import Data.Array as A
import Data.Array.Unsafe as AU
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..), fst, snd)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Trans (ReaderT(), runReaderT)
import Control.Monad.State (State, evalState)
import Control.Monad.State as State
import Data.StrMap (StrMap())
import Control.Monad.Trans (lift)
import Control.MonadPlus.Partial (mrights, mlefts, mpartition)
import Text.Parsing.Parser (PState(..), ParseError(..), ParserT(..), fail,
                            parseFailed, unParserT) as P
import Text.Parsing.Parser.Combinators (option, try, lookAhead, (<?>)) as P
import Text.Parsing.Parser.Pos (Position, initialPos) as P

import Language.Docopt.Argument (Argument(..), Branch(..), isFree, runBranch,
                                prettyPrintArg, prettyPrintArgNaked,
                                hasEnvBacking, getArgument, hasDefault,
                                isRepeatable, isFlag, setRequired) as D
import Language.Docopt.Usage (Usage, runUsage) as D
import Language.Docopt.Env (Env ())
import Language.Docopt.Env as Env
import Language.Docopt.Option as O
import Language.Docopt.Origin as Origin
import Language.Docopt.Origin (Origin())
import Language.Docopt.Value as Value
import Language.Docopt.Value (Value(..))
import Language.Docopt.Parser.Base (getInput)
import Language.Docopt.ParserGen.Token as Token
import Language.Docopt.ParserGen.Token (PositionedToken(..), Token(..),
                                        unPositionedToken, prettyPrintToken)
import Data.String.Ext (startsWith)

debug :: Boolean
debug = false

-- | The value type the parser collects
type RichValueObj = {
  value  :: Value
, origin :: Origin
}

newtype RichValue = RichValue RichValueObj

instance showRichValue :: Show RichValue where
  show (RichValue v) = "RichValue { origin: " ++ show v.origin
                    ++ ", value: " ++ show v.value ++ "}"

instance eqRichValue :: Eq RichValue where
  eq (RichValue v) (RichValue v') = (v.origin == v'.origin) &&
                                    (v.value == v'.value)

unRichValue :: RichValue -> RichValueObj
unRichValue (RichValue o) = o

from :: Origin -> Value -> RichValue
from o v = RichValue $ { value: v, origin: o }

fromArgv :: Value -> RichValue
fromArgv = from Origin.Argv

-- | The output value mappings of arg -> val
type ValueMapping = Tuple D.Argument RichValue

-- | The stateful parser type
type StateObj = { depth :: Int
                , fatal :: Maybe P.ParseError }
type Parser a = P.ParserT (List PositionedToken)
                          (ReaderT Env (State StateObj))
                          a

initialState :: StateObj
initialState = { depth: 0
               , fatal: Nothing }

modifyDepth :: (Int -> Int) -> Parser Unit
modifyDepth f = do
  lift (State.modify \s -> s { depth = f s.depth })

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
command n = token go P.<?> "command " ++ show n
  where
    go (Lit s) | s == n = Just (BoolValue true)
    go _                = Nothing

positional :: String -> Parser Value
positional n = token go P.<?> "positional argument " ++ show n
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

longOption :: O.Name -> (Maybe O.Argument) -> Parser Value
longOption n a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
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
                       in pushed ++ rest
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
            return $ OptParse (Value.read s false) Nothing false
          _  -> case atok of
            Just (Lit s) -> return $ OptParse (Value.read s false)  Nothing true
            otherwise    ->
              if (fromMaybe true (_.optional <<< O.runArgument <$> a))
                 then Right $ OptParse (BoolValue true) Nothing false
                 else Left  $ "Option requires argument: --" ++ n'

    -- case 2:
    -- The name is an exact match and takes no argument
    go (LOpt n' v) _ | isFlag && (n' == n)
      = case v of
             Just _  -> Left $ "Option takes no argument: --" ++ n'
             Nothing -> return $ OptParse (BoolValue true) Nothing false

    -- case 3:
    -- The name is a substring of the input and no explicit argument has been
    -- provdided.
    go (LOpt n' Nothing) _ | not isFlag
      = case stripPrefix n n' of
          Just s -> return $ OptParse (Value.read s false) Nothing false
          _      -> Left "Invalid substring"

    go a b = Left $ "Invalid token" ++ show a ++ " (input: " ++ show b ++ ")"

shortOption :: Char -> (Maybe O.Argument) -> Parser Value
shortOption f a = P.ParserT $ \(P.PState { input: toks, position: pos }) -> do
  return $ case toks of
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
                                        , source:    "-" ++ String.drop 2 s
                                        })
                                newtok
                rest = if hasConsumedArg then LU.tail xs else xs
              in pushed ++ rest
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
          Just s    -> return $ OptParse (Value.read s false) Nothing false
          otherwise -> case atok of
            Just (Lit s) -> return $ OptParse (Value.read s false) Nothing true
            otherwise    ->
              if (fromMaybe true (_.optional <<< O.runArgument <$> a))
                 then Right $ OptParse (BoolValue true) Nothing false
                 else  Left $ "Option requires argument: -" ++ fromChar f'

    -- case 2:
    -- The leading flag matches, there are stacked options, a explicit
    -- argument may have been passed and the option takes an argument.
    go (SOpt f' xs v) _ | (f' == f) && (not isFlag) && (not $ A.null xs)
      = do
        let arg = fromCharArray xs ++ maybe "" ("=" ++ _) v
        return $ OptParse (Value.read arg false)
                          Nothing
                          false

    -- case 3:
    -- The leading flag matches, there are stacked options, the option takes
    -- no argument and an explicit argument has not been provided.
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (not $ A.null xs)
      = return $ OptParse (BoolValue true)
                          (Just $ SOpt (AU.head xs) (AU.tail xs) v)
                          false

    -- case 4:
    -- The leading flag matches, there are no stacked options and the option
    -- takes no argument - total consumption!
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (A.null xs)
      = case v of
              Just _  -> Left $ "Option takes no argument: -" ++ fromChar f'
              Nothing -> return $ OptParse (BoolValue true)
                                            Nothing
                                            false

    go a b = Left $ "Invalid token" ++ show a ++ " (input: " ++ show b ++ ")"

eof :: Parser Unit
eof = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return $ case s of
    Nil -> { consumed: false, input: s, result: Right unit, position: pos }
    (Cons (PositionedToken {token: tok, source}) _) ->
      P.parseFailed s pos
        $ case tok of
              LOpt _ _   -> "Unmatched option: " ++ source
              SOpt _ _ _ -> "Unmatched option: " ++ source
              EOA _      -> "Unmatched option: --"
              Stdin      -> "Unmatched option: -"
              Lit _      -> "Unmatched command: " ++ source


-- | Generate a parser for a single program usage.
genUsageParser :: List D.Usage -- ^ The list of usage specs
               -> Boolean      -- ^ Enable "options-first"
               -> Parser (Tuple D.Branch (List ValueMapping))
genUsageParser xs optsFirst = do
    genBranchesParser (concat $ D.runUsage <$> xs)
                      true
                      optsFirst
                      true

-- | Generate a parser that selects the best branch it parses and
-- | fails if no branch was parsed.
genBranchesParser :: List D.Branch -- ^ The branches to test
                  -> Boolean       -- ^ Expect EOF after each branch
                  -> Boolean       -- ^ Enable "options-first"
                  -> Boolean       -- ^ Can we skip input via fallbacks?
                  -> Parser (Tuple D.Branch (List ValueMapping))
genBranchesParser xs term optsFirst canSkip
  = P.ParserT \(s@(P.PState { input: i, position: pos })) -> do
    env   :: Env      <- ask
    state :: StateObj <- lift State.get

    let
      ps = xs <#> \x -> (Tuple x) <$> do
                          genBranchParser x optsFirst canSkip
                            <* unless (not term) eof
      rs  = evalState (runReaderT (collect s ps) env) initialState
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

      -- check losers first for any fatal errors
      fatal =
          head $ flip filter failures \e ->
            let msg = (unParseError e.result.error).message
             in (startsWith "Option takes no argument" msg) ||
                (startsWith "Option requires argument" msg)


    case fatal of
         (Just e) -> do
            lift $ State.modify (_ {  fatal = Just e.result.error })
            return
              -- XXX: `i` and `pos` are wrong (
              $ P.parseFailed i pos (unParseError e.result.error).message
         otherwise -> do
          maybe'
            (\_ ->
              fromMaybe
                (return do
                  P.parseFailed i pos
                      "The impossible happened. Failure without error")
                (do
                  es <- losers
                  return case es of
                    Cons e es | null es || not (null i) -> do
                      return $ e { result = Left e.result.error }
                    otherwise -> return $ P.parseFailed i pos ""
                )
            )
            (\r -> return $ r { result = Right r.result.value })
            winner

  where
  fixMessage m = m
  collect s ps = ps `flip traverse` \p -> do

    o                 <- P.unParserT p s
    state :: StateObj <- lift State.get

    -- If any parse encountered a fatal error, fail with that error.
    -- XXX: this approach is pragmatic and works around not being able to catch
    --      errors in purescript-parsing. Ideally, this would not be needed.
    --      Also, using "o" is actually misleading here as it claims the error
    --      happened at a different location as to where it actually
    --      did. This has no impact atm on the way neodoc works, but is
    --      something to keep in mind.
    flip fromMaybe
      (do
        e <- state.fatal
        return
          $ return
            $ Left
              -- XXX: use a huge depth for fatal errors to be bubbled
              --      up properly (be selected). Again, this is due to
              --      insufficient control over handling fatal vs.
              --      non-fatal errors.
              $ o { result = { error: e, depth: 99999 } }
      )
      (return $ bimap
        (\e -> o { result = { error: e, depth: state.depth } })
        (\r -> o { result = { value: r, depth: state.depth } })
        o.result)

-- | Generate a parser for a single usage branch
genBranchParser :: D.Branch  -- ^ The branch to match
                -> Boolean   -- ^ Enable "options-first"
                -> Boolean   -- ^ Can we skip input via fallbacks?
                -> Parser (List ValueMapping)
genBranchParser (D.Branch xs) optsFirst canSkip = do
  modifyDepth (const 0) -- reset the depth counter
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
                        -> Parser (List ValueMapping)
    genExhaustiveParser Nil canSkip = return mempty
    genExhaustiveParser ps  canSkip = do
      if debug
        then do
          traceA $ "genExhaustiveParser: "
                ++ (intercalate " " (D.prettyPrintArg <$> ps))
                ++ " - canSkip: " ++  show canSkip
          else return unit
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
            i <- getInput
            traceA $
              "draw: (" ++ (D.prettyPrintArg p) ++ ":"
                        ++ (intercalate ":" (D.prettyPrintArg <$> ps'))
                        ++  ") - n: " ++ show n
                        ++ "from input: "
                        ++ (intercalate " " (prettyPrintToken
                                              <<< _.token
                                              <<< unPositionedToken <$> i))

          -- Generate the parser for the argument `p`. For groups, temporarily
          -- set the required flag to "true", such that it will fail and we have
          -- a chance to retry as part of the exhaustive parsing mechanism
          r <- P.try $ genParser (D.setRequired p true) (n <= 1)

          r' <- P.try do
                  if D.isRepeatable p
                        then draw pss (length pss) (p:tot)
                        else draw ps' (length ps') (p:tot)

          return $ r ++ r'
        ) <|> (defer \_ -> do
              state :: StateObj <- lift State.get
              case state.fatal of
                Just (P.ParseError { message }) -> do
                  P.fail message
                otherwise -> draw (ps' ++ singleton p) (n - 1) tot
              )

        draw ps' n tot | (length ps' > 0) && (n < 0) = do
          env :: StrMap String <- lift ask

          i <- getInput

          let
            vs = ps' <#> \o ->
              maybe
                (Left o)
                (Right <<< Tuple o)
                do
                  (RichValue v) <- do
                    guard (null i || canSkip || (length ps' < length ps))
                    (getEnvValue env o <#> from Origin.Environment) <|>
                    (getDefaultValue o <#> from Origin.Default)     <|>
                    (getEmptyValue   o <#> from Origin.Empty)

                  return $ RichValue v {
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
              return $ fallbacks ++ xs
             else
              if (length missing > 0)
                then P.fail $
                  "Expected option(s): "
                    ++ intercalate ", " (D.prettyPrintArgNaked <$> missing)
                else return fallbacks

          where
          isSkippable (D.Group o bs _)
              = o || (all (all isSkippable <<< D.runBranch) bs)
          isSkippable o = D.isRepeatable o && (any (_ == o) tot)

          getEnvValue :: Env -> D.Argument -> Maybe Value
          getEnvValue env (D.Option (O.Option o@{ env: Just k })) = do
            StringValue <$> Env.lookup k env
          getEnvValue _ _ = Nothing

          getDefaultValue :: D.Argument -> Maybe Value
          getDefaultValue (D.Option (O.Option o@{
              arg: Just (O.Argument { default: Just v })
            })) = return if o.repeatable
                            then ArrayValue $ Value.intoArray v
                            else v
          getDefaultValue _ = Nothing

          getEmptyValue :: D.Argument -> Maybe Value
          getEmptyValue = go
            where
            go (D.Option (O.Option o@{ arg: Nothing }))
              = return
                  $ if o.repeatable then ArrayValue []
                                    else BoolValue false
            go (D.Option (O.Option o@{ arg: Just (O.Argument { optional: true }) }))
              = return
                  $ if o.repeatable then ArrayValue []
                                    else BoolValue false
            go (D.Positional _ r) | r = return $ ArrayValue []
            go (D.Command _ r)    | r = return $ ArrayValue []
            go (D.Stdin)              = return $ BoolValue false
            go (D.EOA)                = return $ ArrayValue []
            go _                      = Nothing

        draw _ _ _ = return mempty

    step :: Acc (List ValueMapping)
         -> D.Argument
         -> Either P.ParseError (Acc (List ValueMapping))

    -- Options always transition to the `Pending state`
    step (Free p) x@(D.Option _)
      = return $ Pending p (singleton x)

    -- "Free" groups always transition to the `Pending state`
    step (Free p) x@(D.Group _ _ _) | D.isFree x
      = return $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      r  <- p
      rs <- genParser x false
      return $ r ++ rs

    -- Options always keep accumulating
    step (Pending p xs) x@(D.Option _)
      = return $ Pending p (x:xs)

    -- "Free" groups always keep accumulating
    step (Pending p xs) x@(D.Group _ _ _) | D.isFree x
      = return $ Pending p (x:xs)

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        r   <- p
        rs  <- genExhaustiveParser xs true
        rss <- genParser y true
        return $ r ++ rs ++ rss

    -- Terminate the parser at the given argument and collect all subsequent
    -- values int an array ("options-first")
    terminate arg = do
      input <- getInput
      let rest = Tuple arg <<< fromArgv <$> do
                  StringValue <<< Token.getSource <$> input
      P.ParserT \(P.PState { position: pos }) ->
        return {
          consumed: true
        , input:    Nil
        , result:   return rest
        , position: pos
        }

    -- Parser generator for a single `Argument`
    genParser :: D.Argument -- ^ The argument to generate a parser for
              -> Boolean    -- ^ Can we skip input via fallbacks?
              -> Parser (List ValueMapping)

    -- Generate a parser for a `Command` argument
    genParser x@(D.Command n r) _ = do
      i <- getInput
      (do
        if r then (some go) else (singleton <$> go)
      ) <|> (P.fail $ "Expected " ++ D.prettyPrintArg x ++ butGot i)
        where go = do Tuple x <<< fromArgv <$> (do
                        v <- command n
                        return if r then ArrayValue $ Value.intoArray v
                                    else v
                      )
                      <* modifyDepth (_ + 1)

    -- Generate a parser for a `EOA` argument
    genParser x@(D.EOA) _ = do
      singleton <<< Tuple x <<< fromArgv <$> (do
        eoa <|> (return $ ArrayValue []) -- XXX: Fix type
        <* modifyDepth (_ + 1)
      ) <|> P.fail "Expected \"--\""

    -- Generate a parser for a `Stdin` argument
    genParser x@(D.Stdin) _ = do
      singleton <<< Tuple x <<< fromArgv <$> (do
        stdin
        <* modifyDepth (_ + 1)
      ) <|> P.fail "Expected \"-\""

    genParser x@(D.Positional n r) _
      | r && optsFirst
      = terminate x

    -- Generate a parser for a `Positional` argument
    genParser x@(D.Positional n r) _ = do
      i <- getInput
      (do
        if r then (some go) else (singleton <$> go)
      ) <|> P.fail ("Expected " ++ D.prettyPrintArg x ++ butGot i)
        where go = do Tuple x <<< fromArgv <$> (do
                        v <- positional n
                        return if r then ArrayValue $ Value.intoArray v
                                    else v
                        )
                      <* modifyDepth (_ + 1)

    genParser x@(D.Group optional bs r) _
      | optsFirst && (length bs == 1) &&
        all (\(D.Branch xs) ->
          case xs of
            Cons (D.Positional n r') Nil -> r' || r
            _                            -> false
        ) bs
      = terminate (LU.head (D.runBranch (LU.head bs)))

    -- Generate a parser for a `Option` argument
    genParser x@(D.Option (O.Option o)) _ = (do
      do
        if o.repeatable then (some go) else (singleton <$> go)
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
                  Tuple x <<< fromArgv <$> (do
                    v <- mkLoptParser o.name o.arg
                    return if o.repeatable then ArrayValue $ Value.intoArray v
                                           else v
                  )
                else if isSopt
                  then P.try do
                    Tuple x <<< fromArgv <$> (do
                      v <- mkSoptParser o.flag o.arg
                      return if o.repeatable then ArrayValue $ Value.intoArray v
                                            else v
                    )
                  else P.fail "long or short option") s
              case o.result of
                  (Left e) -> do
                    when ((startsWith
                            "Option takes no argument"
                            (unParseError e).message)
                       || (startsWith
                            "Option requires argument"
                            (unParseError e).message)) do
                              lift $ State.modify (_ { fatal = Just e })
                    return o
                  otherwise -> return o

          isAnyLopt (LOpt _ _) = return true
          isAnyLopt _          = return false

          isAnySopt (SOpt _ _ _) = return true
          isAnySopt _            = return false

          mkLoptParser (Just n) a = longOption n a
          mkLoptParser Nothing _  = P.fail "long name"

          mkSoptParser (Just f) a = shortOption f a
          mkSoptParser Nothing _  = P.fail "flag"

    -- Generate a parser for an argument `Group`
    -- The total score a group is the sum of all scores inside of it.
    genParser x@(D.Group optional bs repeated) canSkip =
      let mod = if optional then P.option mempty <<< P.try else \p -> p
       in mod go
      where
        go | length bs == 0 = return mempty
        go = do
          x <- step
          if repeated && length (filter (snd >>> from Origin.Argv) x) > 0
             then do
                xs <- go <|> return mempty
                return $ x ++ xs
             else return x

        from o (RichValue v) = v.origin == o
        step = snd <$> do
                genBranchesParser bs
                                  false
                                  optsFirst
                                  -- always allow skipping for non-free groups.
                                  (not (D.isFree x) || canSkip)

    butGot :: List PositionedToken -> String
    butGot (Cons (PositionedToken { source }) _) = ", but got " ++ source
    butGot Nil                                   = ""

unParseError :: P.ParseError -> { position :: P.Position, message :: String }
unParseError (P.ParseError e) = e
