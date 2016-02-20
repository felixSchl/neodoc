-- | Input Parser Generator for Docopt
-- |
-- | > Given a un-ambigious specification as input, generate a parser that can
-- | > be applied to user input.
-- |
-- | ===

module Docopt.Gen.Parser (
  mkApplicationParser
, runCliParser
, CliParser ()
) where

import Prelude
import Control.Plus (empty)
import Debug.Trace
import Control.Monad.State (State(), evalState)
import Control.Apply ((*>), (<*))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.List (List(..), foldM, (:), singleton, some, toList, delete, length
                 , head, many, tail, fromList, filter, reverse, concat)
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Foldable (foldl, intercalate, for_)
import Data.String (fromCharArray, stripPrefix)
import qualified Data.List as L
import qualified Data.List.Unsafe as LU
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Array (uncons)
import Data.Tuple (Tuple(..))
import Data.Monoid (mempty)

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

import Docopt.Types
import Docopt.Pretty
import Docopt.Gen.Types
import Docopt.Gen.Pretty
import Docopt.Spec.Parser.Base (alphaNum, space, getInput, debug)

--------------------------------------------------------------------------------
-- Input Token Parser ----------------------------------------------------------
--------------------------------------------------------------------------------

type CliParser a = P.Parser (List Token) a

runCliParser :: forall a.
                (List Token)
              -> CliParser a
              -> Either P.ParseError a
runCliParser = P.runParser

-- | Test the token at the head of the stream
token :: forall a. (Token -> Maybe a) -> CliParser a
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
  = Free (CliParser a)
  | Pending (CliParser a) (List Argument)

command :: String -> CliParser Value
command n = token go P.<?> "command " ++ show n
  where
    go (Lit s) | s == n = Just (BoolValue true)
    go _                = Nothing

positional :: String -> CliParser Value
positional n = token go P.<?> "positional argument " ++ show n
  where
    go (Lit v) = Just (StringValue v)
    go _       = Nothing

type HasConsumedArg = Boolean
data OptParse = OptParse Value (Maybe Token) HasConsumedArg

longOption :: Name -> (Maybe OptionArgument) -> CliParser Value
longOption n a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case go tok (head xs) of
        Left e -> P.parseFailed toks pos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    (maybe Nil singleton newtok) ++
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
          Just val -> return $ OptParse (StringValue val) Nothing false
          _  -> return case atok of
            Just (Lit s) -> OptParse (StringValue s) Nothing true
            _            ->
              -- return `true` as argument, let caller check
              OptParse (BoolValue true) Nothing false

    -- case 2:
    -- The name is an exact match and takes no argument
    go (LOpt n' _) _ | (takesArg == false) && (n' == n)
      = return $ OptParse (BoolValue true) Nothing false

    -- case 3:
    -- The name is a substring of the input and no explicit argument has been
    -- provdided.
    go (LOpt n' Nothing) atok | takesArg
      = case stripPrefix n n' of
          Just s -> return $ OptParse (StringValue s) Nothing false
          _      -> Left "Invalid substring"

    go _ _ = Left "Invalid token"

shortOption :: Char -> (Maybe OptionArgument) -> CliParser Value
shortOption f a = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case go tok (head xs) of
        Left e -> P.parseFailed toks pos e
        Right (OptParse v newtok hasConsumedArg) ->
          { consumed: maybe true (const false) newtok
          , input:    (maybe Nil singleton newtok) ++
                      (if hasConsumedArg then (LU.tail xs) else xs)
          , result:   Right v
          , position: pos -- ignore pos
          }
    _ -> P.parseFailed toks pos "expected token, met EOF"

  where

    takesArg = isJust a
    def      = maybe Nothing (\(OptionArgument _ v) -> v) a

    -- case 1:
    -- The leading flag matches, there are no stacked options, and an explicit
    -- argument may have been passed.
    go (SOpt f' xs v) atok | (f' == f) && takesArg && (A.length xs == 0)
      = case v of
          Just val -> return $ OptParse (StringValue val) Nothing false
          _  -> return case atok of
            Just (Lit s) -> OptParse (StringValue s) Nothing true
            _ -> OptParse (BoolValue true) Nothing false

    -- case 2:
    -- The leading flag matches, there are stacked options, no explicit
    -- argument has been passed and the option takes an argument.
    go (SOpt f' xs Nothing) _ | (f' == f) && takesArg && (A.length xs > 0)
      = return $ OptParse (StringValue $ fromCharArray xs) Nothing false

    -- case 3:
    -- The leading flag matches, there are stacked options, the option takes
    -- no argument and an explicit argument has not been provided.
    go (SOpt f' xs v) _ | (f' == f) && (takesArg == false) && (A.length xs > 0)
      = return $ OptParse
                (BoolValue true)
                (Just $ SOpt (AU.head xs) (AU.tail xs) v)
                false

    -- case 4:
    -- The leading flag matches, there are no stacked options and the option
    -- takes no argument - total consumption!
    go (SOpt f' xs _) _ | (f' == f) && (takesArg == false) && (A.length xs == 0)
      = return $ OptParse
                (BoolValue true)
                Nothing
                false

    go a b = Left $ "Invalid token " ++ show a ++ " (input: " ++ show b ++ ")"

eof :: CliParser Unit
eof = P.ParserT $ \(P.PState { input: s, position: pos }) ->
  return $ case s of
    Nil -> { consumed: false, input: s, result: Right unit, position: pos }
    _   -> P.parseFailed s pos $
              "Trailing input: "
            ++ (intercalate ", " $ prettyPrintToken <$> s)

-- | Generate a parser for a single program application (Usage).
mkApplicationParser :: Application -> CliParser (List ValueMapping)
mkApplicationParser (Application xs) = do
  P.choice $ P.try <<< mkBranchParser <$> xs
  <* eof

-- | Generate a parser for a single usage branch
mkBranchParser :: Branch -> CliParser (List ValueMapping)
mkBranchParser (Branch xs) = do
  either
    (\_   -> P.fail "Failed to generate parser")
    (\acc -> case acc of
      Free p       -> p
      Pending p xs -> do
        a  <- p
        as <- mkExhaustiveParser xs
        return (a ++ as))
    (foldM step (Free $ pure empty) xs)
  where

    -- Given a list of arguments, try parse them all in any order.
    -- The only requirement is that all input is consumed in the end.
    mkExhaustiveParser :: List Argument -> CliParser (List ValueMapping)
    mkExhaustiveParser Nil = pure empty
    mkExhaustiveParser ps  = do
      draw ps (length ps)
      where
        -- iterate over `ps` until a match `x` is found, then, recursively
        -- apply `draw` until the parser fails, with a modified `ps`.
        draw :: List Argument -> Int -> CliParser (List ValueMapping)
        draw pss@(Cons p ps') n | n >= 0 = (do
          xs  <- mkParser p

          -- verify the arguments for parsed set of options
          -- when an option takes anything but a bool value, i.e. it is not
          -- a switch, an explicit argument *must* be provided.
          let ys = map (\(Tuple a _) -> a) $
                    filter
                      (\(Tuple a v) -> (takesArgument a)
                                    && (not $ isFlag a)
                                    && (isBoolValue v))
                      xs
          if (length ys > 0)
            then P.fail $ "Missing required arguments for "
                        ++ intercalate ", " (prettyPrintArg <$> ys)
            else return unit

          xss <- if isRepeatable p
                      then draw pss (length pss)
                      else draw (ps') (length ps')
          return $ xs ++ xss
        ) <|> (defer \_ -> draw (ps' ++ singleton p) (n - 1))
        draw ps' n | (length ps' > 0) && (n < 0) = do
          let rest = filter
                      (\p -> (not $ isRepeatable p)
                          && (not $ hasDefault p))
                      (reverse ps')
          if (length rest > 0)
            then P.fail $
              "Missing required options: "
                ++ intercalate ", " (prettyPrintArg <$> rest)
            else return Nil
        draw _ _ = return Nil

    -- Options always transition to the `Pending state`
    step (Free p) x@(Option _ _ _ _) = Right $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      a  <- p
      as <- mkParser x
      return $ a ++ as

    -- Options always keep accumulating
    step (Pending p xs) x@(Option _ _ _ _) = Right $
      Pending p (x:xs)

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        a   <- p
        as  <- mkExhaustiveParser xs
        ass <- mkParser y
        return (a ++ as ++ ass)

    -- Parser generator for a single `Argument`
    mkParser :: Argument -> CliParser (List ValueMapping)

    -- Generate a parser for a `Command` argument
    mkParser x@(Command n) = do
      singleton <<< Tuple x <$> do
        command n

    -- Generate a parser for a `Positional` argument
    mkParser x@(Positional n r) = do
      if r then (some go) else (singleton <$> go)
      where go = Tuple x <$> (positional n)

    -- Generate a parser for a `Option` argument
    mkParser x@(Option f n a r) = do
      if r then (some go) else (singleton <$> go)
      where
        go = do
          P.choice $ P.try <$> [
            Tuple x <$> (mkLoptParser n a)
          , Tuple x <$> (mkSoptParser f a)
          ]

        mkLoptParser (Just n) a = longOption n a
        mkLoptParser Nothing _  = P.fail "no long name"

        mkSoptParser (Just f) a = shortOption f a
        mkSoptParser Nothing _  = P.fail "not no flag"

    -- Generate a parser for a argument `Group`
    mkParser (Group optional bs repeated) = do
      concat <$>
        let mod    = if optional then P.option Nil else \p -> p
            parser = if repeated then some go else singleton <$> go
         in mod parser
      where go = P.choice $ P.try <<< mkBranchParser <$> bs
