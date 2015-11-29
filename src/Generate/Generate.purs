module Docopt.Generate (
  mkBranchParser
, runCliParser
, lexArgv
, Token(..)
) where

import Prelude
import Control.Plus (empty)
import Debug.Trace
import Control.Monad.State (State(), evalState)
import Control.Apply ((*>), (<*))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.List (List(..), foldM, (:), singleton, some, toList)
import Data.String (fromCharArray)
import Data.List (many)
import qualified Data.List as L
import qualified Data.Array as A
import Data.Array (uncons)
import Data.Tuple (Tuple(..))
import Data.Monoid (mempty)

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

import Docopt
import Docopt.Parser.Base (alphaNum, space, getInput, debug)

--------------------------------------------------------------------------------
-- Input Lexer
--------------------------------------------------------------------------------

data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | Lit  String

prettyPrintToken :: Token -> String
prettyPrintToken (Lit s) = show s
prettyPrintToken (LOpt n a)
  = "--" ++ n ++ " " ++ (show a)
prettyPrintToken (SOpt n s a)
  = "-"  ++ (fromCharArray (A.cons n s)) ++ " " ++ (show a)

instance showToken :: Show Token where
  show = show <<< prettyPrintToken

parseToken :: P.Parser String Token
parseToken = do
  P.choice $ P.try <$> [ sopt, lopt, lit ]
  <* P.eof
  where
    sopt :: P.Parser String Token
    sopt = do
      P.char '-'
      x  <- alphaNum
      xs <- A.many alphaNum
      arg <- P.choice $ P.try <$> [
        Just <$> do
          many space *> P.char '=' <* many space
          fromCharArray <$> do A.many P.anyChar
      , pure Nothing
      ]
      pure $ SOpt x xs arg
    lopt :: P.Parser String Token
    lopt = do
      P.string "--"
      xs <- fromCharArray <$> do
        A.many alphaNum
      arg <- P.choice $ P.try <$> [
        Just <$> do
          many space *> P.char '=' <* many space
          fromCharArray <$> do A.many P.anyChar
      , pure Nothing
      ]
      pure $ LOpt xs arg
    lit :: P.Parser String Token
    lit = Lit <<< fromCharArray <$> do
      A.many P.anyChar

lexArgv :: (List String) -> Either P.ParseError (List Token)
lexArgv = foldM step Nil
  where
    step :: List Token -> String -> Either P.ParseError (List Token)
    step a b = do
      x <- flip P.runParser parseToken b
      return (a ++ (Cons x Nil))

--------------------------------------------------------------------------------
-- Input Token Parser
--------------------------------------------------------------------------------

type CliParseState = { mapped :: List (Tuple Argument Value) }
type CliParser a = P.ParserT (List Token) (State CliParseState) a

runCliParser :: forall a.
                (List Token)
              -> CliParser a
              -> Either P.ParseError a
runCliParser input =
  flip evalState
  { mapped: Nil }
  <<< P.runParserT
  (P.PState { input: input, position: P.initialPos })

-- | Test the string at the head of the stream
token :: forall a. (Token -> Maybe a) -> CliParser a
token test = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case test tok of
        Just a ->
          let nextpos = pos -- neglect pos
          in
            { consumed: true
            , input:    xs
            , result:   Right a
            , position: nextpos }
        -- XXX: Fix this error message, it makes no sense!
        Nothing -> P.parseFailed toks pos "a better error message!"
    _ -> P.parseFailed toks pos "expected token, met EOF"

-- XXX: Should the `Pending` constructor store a list of `Parsers` instead?
data Acc a
  = Free (CliParser a)
  | Pending (CliParser a) (List Argument)

command :: String -> CliParser Value
command n = token go P.<?> "command"
  where
    go (Lit s) | s == n = Just (BoolValue true)
    go _                = Nothing

positional :: CliParser Value
positional = token go P.<?> "positional"
  where
    go (Lit _) = Just (BoolValue true)
    go _       = Nothing

longOption :: Name -> TakesArgument -> CliParser Value
longOption n b = token go P.<?> "long option"
  where
    -- TOOD: `StringValue` is invalid here! `LOpt` should either have
    --       analysed the type of argument already, so we should be
    --       able to just return `v` / or we need to parse it here?
    go (LOpt n' (Just v)) | (b == true)  && (n' == n) = Just $ StringValue v
    go (LOpt n' Nothing)  | (b == false) && (n' == n) = Just $ BoolValue   true
    go _                                              = Nothing

-- | Parse the token at the head of the input stream and possibly replace
-- | it with another token. Consider, e.g. an option was parsed but has a
-- | valid remainder, that remainder should be offered to subsequent parsers.
token' :: forall a. (Token -> Tuple (Maybe Token) (Maybe a)) -> CliParser a
token' f = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons tok xs ->
      case f tok of
        Tuple remainder (Just a) ->
          let nextpos = pos -- neglect pos
          in {
            consumed: maybe true (const false) remainder
          , input:    maybe xs (\r -> (r:xs)) remainder
          , result:   Right a
          , position: nextpos
          }
        _ -> P.parseFailed toks pos "bad token"
    _ -> P.parseFailed toks pos "expected token, met EOF"

shortOption :: Char -> (Maybe OptionArgument) -> CliParser Value
shortOption f a = token' go P.<?> "short option"
  where

    takesArg = isJust a
    def      = maybe Nothing (\(OptionArgument _ v) -> v) a

    -- case 1:
    -- The leading flag matches, there are no stacked options, but an explicit
    -- argument has been passed.
    go (SOpt f' xs (Just v)) | (f' == f) && takesArg
      = case uncons xs of
            Nothing -> Tuple Nothing (Just $ StringValue v)
            _       -> Tuple Nothing Nothing

    -- case 2:
    -- The leading flag matches, there are stacked options and no explicit
    -- argument has been passed
    go (SOpt f' xs Nothing) | (f' == f) && takesArg
      = case uncons xs of
          Just _  -> Tuple Nothing (Just $ StringValue $ fromCharArray xs)
          Nothing -> case def of
            Just val -> Tuple Nothing (Just val)
            Nothing  -> Tuple Nothing Nothing

    -- case 3:
    -- The leading flag matches and the option takes no argument
    go (SOpt f' xs Nothing) | (f' == f) && (takesArg == false)
      = case uncons xs of
          Just _  -> Tuple Nothing Nothing
          Nothing -> Tuple Nothing (Just $ BoolValue true)

    -- case 4:
    -- A option in the stack matches and takes no argument
    go (SOpt f xs v) | (takesArg == false)
      = case A.elemIndex f xs of
          Just i  ->
            maybe
              (Tuple Nothing Nothing)
              (\xs' -> Tuple (Just $ SOpt f xs' v) (Just $ BoolValue true))
              (A.deleteAt i xs)
          Nothing -> Tuple Nothing Nothing

    -- case 5:
    -- A option in the stack matches and takes an argument, however
    -- an explicit argument is present. In this case, the only valid option is
    -- the last element in the option stack!
    go (SOpt f xs (Just v)) | takesArg
      = case A.elemIndex f xs of
          Just i | (i == (A.length xs - 1)) ->
            Tuple
              (Just $ SOpt f (maybe [] id (A.init xs)) Nothing)
              (Just $ StringValue v)
          _ -> Tuple Nothing Nothing

    -- case 6:
    -- A option in the stack matches and takes an argument and no
    -- explicit argument is present
    go (SOpt f xs Nothing) | takesArg
      = case A.elemIndex f xs of
          Just i ->
            let ys = A.drop (i + 1) xs
            in if (A.length ys > 0)
              then
                Tuple
                  (Just $ SOpt f (A.take i xs) Nothing)
                  (Just $ StringValue $ fromCharArray ys)
              else case def of
                Just val -> Tuple Nothing (Just val)
                Nothing  -> Tuple Nothing Nothing
          _ -> Tuple Nothing Nothing

    go o = Tuple (Just o) Nothing

-- Notes and thoughts:
--
-- CMD OPT POS OPT OPT OPT
--             ^---------^
--                  `- interchangeable
--
-- When needle is on
--      CMD => Generate parser
--      POS => Generate parser
--      GRP => Generate parser recursively
--      OPT => Start accumlating
--
mkBranchParser :: Branch -> CliParser (List (Tuple Argument Value))
mkBranchParser (Branch xs) = do
  either
    (\_ -> P.fail "Failed to generate parser")
    (\acc -> case acc of
      Free p       -> p
      Pending p xs -> do
        a  <- p
        as <- mkExaustiveParser xs
        return (a ++ as))
    (foldM step (Free $ pure empty) xs)
  where

    -- Given a list of arguments, try parse them all in any order.
    -- The only requirement is that all input is consumed in the end.
    mkExaustiveParser :: List Argument
                      -> CliParser (List (Tuple Argument Value))
    mkExaustiveParser Nil = pure empty

    -- XXX: IMPLEMENT THIS!
    mkExaustiveParser ps  = pure empty

    -- Options always transition to the `Pending state`
    step (Free p) x@(Option _ _ _ _) = Right $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      a  <- p
      as <- (mkParser x)
      pure (a ++ as)

    -- Options always keep accumulating
    step (Pending p xs) x@(Option _ _ _ _) = Right $
      Pending p (x:xs)

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        a  <- p
        -- TODO: Create a parser that continues to consume elements from a list
        --       until the list is exhausted! The parser for each `x` in `xs`
        --       can be retrieved by `mkParser <$> xs`, however the tricky part is
        --       parsing the list until it has been totally consumed.
        -- as <- ???
        pure empty

    -- Parser generator for a single `Argument`
    mkParser :: Argument -> CliParser (List (Tuple Argument Value))

    -- Generate a parser for a `Command` argument
    mkParser x@(Command n) = do
      singleton <<< Tuple x <$> do
        command n

    -- Generate a parser for a `Positional` argument
    mkParser x@(Positional n r) = do
      if r then (some go) else (singleton <$> go)
      where go = Tuple x <$> positional

    -- Generate a parser for a `Option` argument
    mkParser x@(Option f n a r) = do
      if r then (some go) else (singleton <$> go)
      where
        go = do
          P.choice $ P.try <$> [
            Tuple x <$> (mkLoptParser n a)
          , Tuple x <$> (mkSoptParser f a)
          ]

        mkLoptParser (Just n) a = longOption n (isJust a)
        mkLoptParser Nothing _  = P.fail "no long name"

        mkSoptParser Nothing _  = P.fail "not no flag"
        mkSoptParser (Just f) a = shortOption f a

    -- Generate a parser for a argument `Group`
    mkParser (Group o bs r) = 
      -- XXX: Recursively generate a parser.
      --      NOTE: THIS COMPUTATION MAY FAIL:
      --            `mkParser` must be in the Either monad
      -- XXX: Consider `r` here - we must either return a list, always, or
      --      a data type that ensures that at least one value is present:
      --      `X Xs`, where both `X` and `Xs` are data constructors.
      pure empty
