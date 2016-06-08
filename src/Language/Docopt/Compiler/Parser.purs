-- | Input Parser Generator for Docopt
-- |
-- | > Given a un-ambigious specification as input, generate a parser that can
-- | > be applied to user input.
-- |
-- | ===

module Language.Docopt.Compiler.Parser (
    spec
  , initialState
  , Parser ()
  , StateObj ()
  , ValueMapping ()
  , Options ()
  ) where

import Prelude
import Control.Monad.RWS (RWS(), evalRWS)
import Control.Plus (empty)
import Debug.Trace
import Control.Apply ((<*), (*>))
import Data.Function (on)
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe, fromMaybe, maybe', isNothing)
import Data.List (List(..), reverse, singleton, concat, length, (:),
                  some, filter, head, fromList, sortBy, groupBy, last, null,
                  tail, many)
import Data.List.Lazy (take, repeat, fromList) as LL
import Control.Alt ((<|>))
import Data.Traversable (traverse, for)
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
import Text.Parsing.Parser.Combinators (option, try, lookAhead, (<?>), choice) as P
import Text.Parsing.Parser.Pos (Position) as P

import Language.Docopt.Argument (Argument(..), Branch, isFree,
                                prettyPrintArg, prettyPrintArgNaked,
                                isRepeatable, OptionArgumentObj(),
                                setRequired, isOptional, isGroup
                                ) as D
import Language.Docopt.Usage (Usage, runUsage) as D
import Language.Docopt.Env (Env ())
import Language.Docopt.Env as Env
import Language.Docopt.Origin as Origin
import Language.Docopt.Origin (Origin())
import Language.Docopt.Value as Value
import Language.Docopt.Value (Value(..))
import Language.Docopt.RichValue (RichValue(..), unRichValue, prettyPrintRichValue)
import Language.Docopt.RichValue (from, getOrigin) as RValue
import Language.Docopt.Parser.Base (getInput)
import Language.Docopt.Compiler.Token (getSource) as Token
import Language.Docopt.Compiler.Token (PositionedToken(..), Token(..),
                                        unPositionedToken, prettyPrintToken)
import Data.String.Ext (startsWith)

-- | Toggle debugging on/off during development
debug :: Boolean
debug = false

-- | The output value mappings of arg -> val
type ValueMapping = Tuple D.Argument RichValue

-- | The stateful parser type
type StateObj = { depth :: Int, done :: Boolean }

-- | The CLI parser
type Parser a = P.ParserT (List PositionedToken)
                          (RWS Env Unit StateObj)
                          a

initialState :: StateObj
initialState = { depth: 0, done: false }

modifyDepth :: (Int -> Int) -> Parser Unit
modifyDepth f = lift (State.modify \s -> s { depth = f s.depth })

markDone :: Parser Unit
markDone = lift (State.modify \s -> s { done = true })

-- | The options for generating a parser
type Options r = {
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
stdin = token go P.<?> "-"
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

-- | Parse user input against a program specification.
spec
  :: forall r
   . List D.Usage -- ^ the list of usage branches
  -> Options r    -- ^ generator options
  -> Parser (Tuple D.Branch (List ValueMapping))
spec xs options = do
  let
    -- Create a parser for each usage line.
    parsers
      = (concat $ D.runUsage <$> xs) <#> \branch -> do
          vs <- exhaustP options true false 0 branch
          pure (Tuple branch vs)
          <* eof

  -- Evaluate the parsers, selecting the result with the most
  -- non-substituted values.
  evalParsers parsers \(Tuple _ vs) ->
    sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs

-- Parse a list of arguments.
-- Take care of clumping free vs. non-free (fixed) arguments.
exhaustP
  :: forall r
   . Options r       -- ^ generator options
  -> Boolean         -- ^ can we skip using fallback values?
  -> Boolean         -- ^ are we currently skipping using fallback values?
  -> Int             -- ^ recursive level
  -> List D.Argument -- ^ the list of arguments to parse
  -> Parser (List ValueMapping)
exhaustP options skippable isSkipping l xs = do
  _debug \_->
       "exhaustP: "     <> (intercalate " " $ D.prettyPrintArg <$> xs)
    <> ", skippable: "  <> show skippable
    <> ", isSkipping: " <> show isSkipping

  concat <$>
    for (clump xs)
        (clumpP options
                skippable
                isSkipping
                l)

  where
  _debug s = if debug
                then traceA $ indentation <> (s unit)
                else pure unit
  indentation :: String
  indentation = fromCharArray $ LL.fromList $ LL.take (l * 4) $ LL.repeat ' '

data Clump a = Free a | Fixed a

instance showClump :: (Show a) => Show (Clump a) where
  show (Fixed a) = "Fixed " <> show a
  show (Free  a) = "Free "  <> show a

isFree :: forall a. Clump a -> Boolean
isFree (Free _) = true
isFree _        = false

prettyPrintArgClump :: Clump (List D.Argument) -> String
prettyPrintArgClump (Fixed xs) = "Fixed " <> (intercalate " " $ D.prettyPrintArg <$> xs)
prettyPrintArgClump (Free  xs) = "Free "  <> (intercalate " " $ D.prettyPrintArg <$> xs)

-- Clump together arguments to aid parsing "invisible" subgroups, where
-- arguments may appear in any order, for example. "Fixed" argument lists
-- are parsed in fixed order, whereas "Free" argument lists, can be parsed in
-- any order.
clump
  :: List D.Argument -- ^ the list of arguments to clump
  -> List (Clump (List D.Argument))
clump xs = reverse $ foldl go Nil xs
  where
  go (Nil) x = pure $ (if D.isFree x then Free else Fixed) $ singleton x
  go   (Cons (Free a)  zs) x | D.isFree x = (Free (a ++ (singleton x))) : zs
  go u@(Cons (Free _)   _) x = (Fixed $ singleton x) : u
  go u@(Cons (Fixed _)  _) x | D.isFree x = (Free $ singleton x): u
  go   (Cons (Fixed a) zs) x = (Fixed (a ++ (singleton x))) : zs

data Required a = Required a | Optional a

unRequired :: forall a. Required a -> a
unRequired (Required a) = a
unRequired (Optional a) = a

isRequired :: forall a. Required a -> Boolean
isRequired (Required _) = true
isRequired _            = false

toOptional :: forall a. Required a -> Required a
toOptional (Required a) = Optional a
toOptional (Optional a) = Optional a

prettyPrintRequiredArg :: Required D.Argument -> String
prettyPrintRequiredArg (Required x) = "Required " <> D.prettyPrintArg x
prettyPrintRequiredArg (Optional x) = "Optional " <> D.prettyPrintArg x

-- Parse a clump of arguments.
clumpP
  :: forall r
   . Options r               -- ^ the parsing options
  -> Boolean                 -- ^ can we skip using fallback values?
  -> Boolean                 -- ^ are we currently skipping using fallback values?
  -> Int                     -- ^ recursive level
  -> Clump (List D.Argument) -- ^ the clumps of arguments to parse.
  -> Parser (List ValueMapping)
clumpP options skippable isSkipping l c = do

  i <- getInput
  _debug \_->
       "parsing clump: " <> prettyPrintArgClump c
    <> ", skippable: "   <> show skippable
    <> ", isSkipping: "  <> show isSkipping
    <> ", input: "       <> show (intercalate " " $ Token.getSource <$> i)

  go skippable isSkipping l c
  where
  go _         _          l (Fixed xs) = concat <$> for xs (argP' options false false l)
  go skippable isSkipping l (Free  xs) = draw (Required <$> xs) (length xs)
    where

    draw :: List (Required D.Argument) -> Int -> Parser (List ValueMapping)

    -- Recursively apply each argument parser.
    -- Should an argument be repeatable, try parsing any adjacent matches
    -- repeatedly (NOTE: this could interfere with `argP`'s repetition handling?)
    draw xss@(Cons x xs) n | n >= 0 = (do

      i <- getInput
      _debug \_->
           "draw: "         <> (intercalate " " $ prettyPrintRequiredArg <$> xss)
        <> ", n: "          <> show n
        <> ", skippable: "  <> show skippable
        <> ", isSkipping: " <> show isSkipping
        <> ", input: "      <> show (intercalate " " $ Token.getSource <$> i)

      -- Try parsing the argument 'x'. If 'x' fails, enqueue it for a later try.
      -- Should 'x' fail and should 'x' be skippable (i.e. it defines a default
      -- value or is backed by an environment variable), substitute x.
      -- For groups, temporarily set the required flag to "true", such that it
      -- will fail and we have a chance to retry as part of the exhaustive
      -- parsing mechanism.

      let
        arg = unRequired x
        mod = if isRequired x then id else P.option Nil

      -- parse the next argument. failure will cause the argument to be put at
      -- the back of the queue and the number of retries to be cut down by one.
      vs <- P.try do
              mod do
                argP' options     -- the parsing options
                      isSkipping  -- propagate the 'isSkipping' property
                      false       -- reset 'isSkipping' to false
                      (l + 1)     -- increase the recursive level
                      (D.setRequired arg true)

      _debug \_->
        "draw: matched: " <> (intercalate ", " $
          (vs) <#> \(Tuple k v) ->
            D.prettyPrintArg k <> " => " <> prettyPrintRichValue v
        )

      vss <- P.try do
        if (D.isRepeatable arg &&
          length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
            then do
              _debug \_->
                "draw: repeating as optional: "
                  <> D.prettyPrintArg arg
              -- Make successive matches of this repeated group optional.
              draw (xs <> pure (toOptional x)) (length xss)
            else draw xs (length xs)

      _debug \_->
        "draw: matched (2): " <> (intercalate ", " $
          (vs <> vss) <#> \(Tuple k v) ->
            D.prettyPrintArg k <> " => " <> prettyPrintRichValue v
        )

      pure (vs <> vss)
    ) <|> (defer \_ -> do
      _debug \_->
           "draw: failure"
        <> ", requeueing: "  <> prettyPrintRequiredArg x
      draw (xs <> singleton x) (n - 1)
    )

    -- All options have been matched (or have failed to be matched) at least
    -- once by now. See where we're at - is there any required option that was
    -- not matched at all?
    draw xss n | (length xss > 0) && (n < 0) = do

      i <- getInput
      _debug \_->
           "draw: edge-case"
        <> ", left-over: "  <> (intercalate " " $ prettyPrintRequiredArg <$> xss)
        <> ", n: "          <> show n
        <> ", skippable: "  <> show skippable
        <> ", isSkipping: " <> show isSkipping
        <> ", input: "      <> show (intercalate " " $ Token.getSource <$> i)

      env <- lift ask

      let
        vs = xss <#> \x -> do
          let arg = unRequired x
          maybe
            (Left x)
            (Right <<< Tuple x)
            do
              v <- unRichValue <$> getFallbackValue env arg
              pure $ RichValue v {
                value = if D.isRepeatable arg
                    then ArrayValue $ Value.intoArray v.value
                    else v.value
              }
        missing = filter (\x ->
          let arg = unRequired x
           in isRequired x &&
              -- This may look very counter-intuitive, yet getting fallback
              -- values for entire groups is not possible and not logical.
              -- If a group that is allowed to be omitted fails, there won't
              -- be any values to fall back onto.
              not (D.isGroup arg && D.isOptional arg)
        ) (mlefts vs)
        fallbacks = mrights vs

      _debug \_->
           "draw: missing:" <> (intercalate " " $ prettyPrintRequiredArg <$> missing)

      if isSkipping && length missing > 0
        then
          P.fail $
            "Expected "
              <> intercalate ", "
                  (D.prettyPrintArgNaked <<< unRequired <$> missing)
        else
          if skippable || null i
            then do
              if not isSkipping
                then exhaustP options true true l (unRequired <$> xss)
                else pure ((unRequired `lmap` _) <$> fallbacks)
            else
              P.fail $
                "Expected "
                  <> intercalate ", "
                      (D.prettyPrintArgNaked <<< unRequired <$> xss)

    draw _ _ = pure Nil

  _debug s = if debug
                then traceA $ indentation <> (s unit)
                else pure unit
  indentation :: String
  indentation = fromCharArray $ LL.fromList $ LL.take (l * 4) $ LL.repeat ' '

-- Parse a single argument from argv.
argP'
  :: forall r
   . Options r
  -> Boolean    -- ^ can we skip using fallback values?
  -> Boolean    -- ^ are we currently skipping using fallback values?
  -> Int        -- ^ recursive level
  -> D.Argument -- ^ the argument to parse
  -> Parser (List ValueMapping)
argP' options skippable isSkipping l x = do
  state :: StateObj <- lift State.get
  if state.done
     then pure Nil
     else argP options skippable isSkipping l x

-- Parse a single argument from argv.
argP
  :: forall r
   . Options r
  -> Boolean    -- ^ can we skip using fallback values?
  -> Boolean    -- ^ are we currently skipping using fallback values?
  -> Int        -- ^ recursive level
  -> D.Argument -- ^ the argument to parse
  -> Parser (List ValueMapping)

-- Terminate at singleton groups that house only positionals.
argP options _ _ _ x@(D.Group (grp@{ branches: Cons branch Nil }))
  | options.optionsFirst &&
      case branch of
        Cons (D.Positional pos) Nil -> pos.repeatable || grp.repeatable
        _                           -> false
  = terminate (LU.head branch) true

-- Terminate at option if part of 'stopAt'
argP options _ _ _ x@(D.Option o) |
  let names = A.catMaybes [ ("--" ++ _) <$> o.name
                          , ("-"  ++ _) <<< fromChar <$> o.flag
                          ]
    in any (_ `elem` options.stopAt) names
  = terminate x false

-- The recursive branch of the argv argument parser.
-- For groups, each branch is run and analyzed.
argP options skippable isSkipping l g@(D.Group grp) = do
  -- Create a parser for each branch in the group
  let parsers = exhaustP options skippable isSkipping l <$> grp.branches

  -- Evaluate the parsers, selecting the result with the most
  -- non-substituted values.
  vs <- (if grp.optional then P.option Nil else id) do
    evalParsers parsers (\vs ->
      sum $ (Origin.weight <<< _.origin <<< unRichValue <<< snd) <$> vs
    )

  vss <- if (grp.repeatable &&
          length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
            then loop Nil
            else pure Nil

  pure $ vs <> vss

  where
    loop acc = do
      vs <- argP' options skippable isSkipping l (D.Group grp {  optional = true })
      if (length (filter (snd >>> isFrom Origin.Argv) vs) > 0)
        then loop $ acc <> vs
        else pure acc

argP options _ _ _ x@(D.Positional pos)
  | pos.repeatable && options.optionsFirst = terminate x true

-- The non-recursive branch of the argv argument parser.
-- All of these parsers consume one or more adjacent arguments from argv.
argP _ _ _ _ x = getInput >>= \i -> (
  (if D.isRepeatable x then some else liftM1 singleton) do
    Tuple x <<< (RValue.from Origin.Argv) <$> do
      P.ParserT \s -> do
        o <- P.unParserT (go x) s

        -- Check error messages for fatal errors.
        -- XXX: This should also change.
        case o.result of
          (Left e) -> do
            let err = unParseError e
            if ((startsWith "Option takes no argument" err.message)
             || (startsWith "Option requires argument" err.message))
              then do
                pure $ o {
                  result = Left $ P.ParseError $ err {
                    fatal = true
                  }
                }
              else pure o
          otherwise -> pure o
    <* modifyDepth (_ + 1)
  ) <|> P.fail ("Expected " <> D.prettyPrintArg x <> butGot i)

  where
  go (D.Positional pos) = positional pos.name
  go (D.Command    cmd) = command    cmd.name
  go (D.Stdin         ) = stdin
  go (D.EOA           ) = eoa <|> (pure $ ArrayValue [])
  go (D.Option       o) = do
    -- Perform a pre-cursory test in order to capture relevant error
    -- messags which would otherwise be overriden (e.g. a meaningful
    -- error message when trying to match a LOpt against a LOpt would
    -- be overridden by a meaningless try to match a SOpt against a
    -- a LOpt).
    isLoptAhead <- P.option false (P.lookAhead $ P.try $ token isAnyLopt)
    isSoptAhead <- P.option false (P.lookAhead $ P.try $ token isAnySopt)

    case o.name of
      Just n  | isLoptAhead -> longOption n o.arg
      otherwise ->
        case o.flag of
          Just c | isSoptAhead -> shortOption c o.arg
          otherwise -> do
            P.fail "Expected long or short option"
    where
    isAnyLopt (LOpt _ _)   = pure true
    isAnyLopt _            = pure false
    isAnySopt (SOpt _ _ _) = pure true
    isAnySopt _            = pure false

  butGot (Cons (PositionedToken { source }) _) = ", but got " <> source
  butGot Nil                                   = ""

-- Evaluate multiple parsers, producing a new parser that chooses the best
-- succeeding match or none.
evalParsers
  :: forall a b
   . (Show a, Ord b)
  => List (Parser a)
  -> (a -> b)
  -> Parser a
evalParsers parsers p = do
  P.ParserT \(pState@(P.PState { input: i, position: pos })) -> do
    env   :: Env      <- ask
    state :: StateObj <- State.get

    let
      -- Run each parser in the underlying monad and capture any errors
      -- that occur as well as the result. This essentially circumvents
      -- `ParserT`'s bind instance.
      collect = for parsers \parser -> do
        -- reset the depth for every branch
        o <- P.unParserT (modifyDepth (const 0) *> parser) pState
        state' :: StateObj <- State.get
        pure $ bimap
          (\e -> o { result = { error: e, depth: state'.depth + state.depth } })
          (\r -> o { result = { value: r, depth: state'.depth + state.depth } })
          o.result

      -- Evaluate the parsers.
      results    = fst $ evalRWS collect env initialState
      successes  = mrights results
      successes' = reverse successes
      failures   = mlefts results

      -- Evaluate any failed parses
      losers = last
        $ groupBy (eq `on` (_.depth <<< _.result))
                  (sortBy (compare `on` (_.depth <<< _.result))
                          failures)

      -- Check for any fatal errors. These must take priority!
      -- The first fatal error to occur takes priority.
      fatal = head
        $ filter (\x -> _.fatal $ unParseError (x.result.error))
                  failures

      -- Evaluate any winning candidates.
      -- First, sort the results by their depth, then group consecutive
      -- elements, take the highest element and sort the inner values by using
      -- the user provided function.
      winner = do
        x <- last $ groupBy (eq `on` (_.depth <<< _.result))
                        (sortBy (compare `on` (_.depth <<< _.result))
                                successes')
        maximumBy (compare `on` (\y -> p y.result.value)) x

    -- Finally, evaluate the results!
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

 -- Terminate the parser at the given argument and collect all subsequent
-- values int an array ("options-first")
terminate :: D.Argument -> Boolean -> Parser (List ValueMapping)
terminate arg includeSelf = do
  input <- getInput
  let rest = Tuple arg <<< (RValue.from Origin.Argv) <$> do
                StringValue <<< Token.getSource <$>
                  if includeSelf
                       then input
                       else fromMaybe Nil (tail input)
  markDone
  modifyDepth (const 99999)
  P.ParserT \(P.PState { position: pos }) ->
    pure {
      consumed: true
    , input:    Nil
    , result:   pure rest
    , position: pos
    }

-- Find a fallback value for the given argument.
getFallbackValue :: Env -> D.Argument -> Maybe RichValue
getFallbackValue env x = do
  (fromEnv     x <#> RValue.from Origin.Environment) <|>
  (fromDefault x <#> RValue.from Origin.Default)     <|>
  (empty       x <#> RValue.from Origin.Empty)

  where
  fromEnv :: D.Argument -> Maybe Value
  fromEnv (D.Option (o@{ env: Just k })) = StringValue <$> Env.lookup k env
  fromEnv _                              = Nothing

  fromDefault :: D.Argument -> Maybe Value
  fromDefault (D.Option (o@{ arg: Just { default: Just v } }))
    = pure if o.repeatable
              then ArrayValue $ Value.intoArray v
              else v
  fromDefault _ = Nothing

  empty :: D.Argument -> Maybe Value
  empty = go
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


isFrom :: Origin -> RichValue -> Boolean
isFrom o rv = o == RValue.getOrigin rv

unParseError :: P.ParseError -> { position :: P.Position, message :: String, fatal :: Boolean }
unParseError (P.ParseError e) = e
