module Docopt.Generate (
  mkBranchParser
, runCliParser
, lexArgv
, Value(..)
, Token(..)
) where

import Prelude
import Control.Plus (empty)
import Debug.Trace
import Control.Monad.State (State(), evalState)
import Control.Apply ((*>), (<*))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.List (List(..), foldM, (:), singleton, some)
import Data.String (fromCharArray)
import Data.List (many)
import qualified Data.List as L
import qualified Data.Array as A
import Data.Tuple (Tuple(..))
import Data.Monoid (mempty)

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

import Docopt
import Docopt.Parser.Base (alphaNum, space)

--------------------------------------------------------------------------------
-- Input Lexer
--------------------------------------------------------------------------------

data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | Lit  String

prettyPrintToken :: Token -> String
prettyPrintToken (LOpt n a)   = "--" ++ n ++ " " ++ (show a)
prettyPrintToken (SOpt n s a) = "-"  ++ (fromCharArray (A.cons n s)) ++ " " ++ (show a)
prettyPrintToken (Lit s)      = show s

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
      pure (x:a)

--------------------------------------------------------------------------------
-- Input Token Parser
--------------------------------------------------------------------------------

data Value
  = StringValue String
  | BoolValue   Boolean
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

instance showValue :: Show Value where
  show (StringValue s) = "StringValue " ++ s
  show (BoolValue b)   = "BoolValue "   ++ (show b)

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
    (\_ -> pure empty)
    (\_ -> pure empty)
    (foldM step (Free $ pure empty) xs)
  where
    -- Options always transition to the `Pending state`
    step (Free p) x@(Option _ _ _ _ _) = Right $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      a  <- p
      as <- (mkParser x)
      pure (a ++ as)

    -- Options always keep accumulating
    step (Pending p xs) x@(Option _ _ _ _ _) = Right $
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

    mkParser :: Argument -> CliParser (List (Tuple Argument Value))

    mkParser x@(Command n) = do
      singleton <<< Tuple x <$> do
        command n

    mkParser x@(Positional n r) = do
      if r then (some go) else (singleton <$> go)
      where go = Tuple x <$> positional

    mkParser (Option f n a d r) = do
      -- XXX: Match against input:
      --    ["--fooBAR", ...] == ["--foo", "BAR"]
      --    ["-fFILE", ...]   == ["-f", "FILE", ...]
      --
      --    1. See if this option's long name exists and if it is a substring
      --        of the input, or
      --    2. see if this option's long name exists and if it is a substring
      --        of the input.
      --    3. If match is found, consume next input string from stream.
      -- XXX: Consider `r` here - we must either return a list, always, or
      --      a data type that ensures that at least one value is present:
      --      `X Xs`, where both `X` and `Xs` are data constructors.
      pure empty

    mkParser (Group o bs r) = 
      -- XXX: Recursively generate a parser.
      --      NOTE: THIS COMPUTATION MAY FAIL:
      --            `mkParser` must be in the Either monad
      -- XXX: Consider `r` here - we must either return a list, always, or
      --      a data type that ensures that at least one value is present:
      --      `X Xs`, where both `X` and `Xs` are data constructors.
      pure empty
