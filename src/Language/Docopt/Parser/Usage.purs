module Language.Docopt.Parser.Usage (
    usageParser
  , run
  , parse
  , module U
  ) where

import Prelude
import Debug.Trace
import Data.Functor (($>))
import Language.Docopt.Parser.Lexer as L
import Language.Docopt.Parser.Usage.Option as O
import Language.Docopt.Parser.Usage.Usage as U
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Bind ((=<<))
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Data.Either (Either)
import Data.List (List(..), many, some, singleton, length, modifyAt)
import Data.Maybe (fromMaybe, Maybe(..), maybe, isNothing)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (tuple3)
import Language.Docopt.Parser.Common (markIndent', markLine, indented,
                                     sameIndent, lessIndented, moreIndented)
import Language.Docopt.Parser.Usage.Argument (Argument(..))
import Language.Docopt.Parser.Usage.Usage (Usage(..))
import Text.Parsing.Parser (ParseError) as P
import Text.Parsing.Parser.Combinators (try, optional, choice, sepBy1, between,
                                       optionMaybe, lookAhead) as P
import Text.Parsing.Parser.Combinators ((<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(Position)) as P

-- | TokenParser to parse the usage section
-- |
-- | This parser is tricky because it has to solve the following problems:
-- |    * What is the program name?
-- |    * Disambiguate between a usage and random text.
-- |    * Disambiguate between a usage and a line-wrapped usage.
-- |
-- | Hence, the following approach is employed:
-- |    * The parser expects a valid program name as first token
-- |    * The parser marks the indent of the first program name
-- |    * Tokens past the identation mark are considered to be part of the
-- |      previous usage pattern.
-- |    * Tokens before the identation mark are NOT considered interesting and
-- |      are IGNORED.
-- |    * A token at the identation mark starts a new usage pattern parse.
-- |
usageParser :: Boolean -- ^ Enable "smart-options" parsing
            -> L.TokenParser (List Usage)
usageParser smartOpts = do

  -- Calculate and mark the original program indentation.
  P.Position { column: startCol } <- L.nextTokPos <?> "Program name"
  name <- program
  markLine do
    markIndent' startCol $ do
      Cons
      <$> (usageLine name)
      <*> many do
            P.optional $ P.try do
              L.name >>= guard <<< (_ == "or")
              L.colon
            program
            usageLine name
  <* (L.eof <?> "End of usage section")

  where

    usageLine :: String -> L.TokenParser Usage
    usageLine name = Usage name <$> do
      xs <- "Option, Positional, Command, Group or Reference elements" <??> do
                  (many $ P.try $ moreIndented *> elem) `P.sepBy1` L.vbar
      eoa <- P.choice [
        P.try $ do
          maybeInParens do
            maybeInParens do
              L.doubleDash
              many elem
            many elem
          many elem
          pure $ Just EOA
      , (do
          L.eof <|> (P.lookAhead $ lessIndented <|> sameIndent)
          pure Nothing
        )
        -- XXX: We could show the last token that failed to be consumed, here
        <?> "End of usage line"
      ]

      -- Push the EOA onto the last branch (the most right branch)
      pure $ maybe xs id do
        e <- eoa
        modifyAt
          (length xs - 1)
          (\as -> as <> (singleton e))
          xs

    maybeInParens p = do
      Tuple close v <- moreIndented *> do
        Tuple
          <$> (P.optionMaybe $ P.choice [ L.lparen  *> pure L.rparen
                                        , L.lsquare *> pure L.rsquare ])
          <*> p
      fromMaybe (pure unit) close
      pure v

    elem :: L.TokenParser Argument
    elem = defer \_ -> do
      P.choice $ P.try <$>
        [ option
        , positional
        , command
        , (if smartOpts then trySmartOpt else id) <$> group
        , reference
        , stdin
        ] <?> "Option, Positional, Command, Group or Reference"

    trySmartOpt :: Argument -> Argument
    trySmartOpt grp@(Group oo bs r) = fromMaybe grp $ do
      Tuple opt optarg <- case bs of
                              (Cons (Cons opt' (Cons arg' Nil)) Nil) ->
                                pure $ Tuple opt' arg'
                              otherwise -> Nothing

      optf <- do
        case opt of
              (Option o) | isNothing o.arg ->
                pure $ \argName isArgOptional isRepeatable ->
                  Option $ o {
                    arg = pure {
                      name:     argName
                    , optional: isArgOptional
                    }
                  , repeatable = isRepeatable
                  }
              (OptionStack o) | isNothing o.arg ->
                pure $ \argName isArgOptional isRepeatable ->
                  OptionStack $ o {
                    arg = pure {
                      name:     argName
                    , optional: isArgOptional
                    }
                  , repeatable = isRepeatable
                  }
              otherwise -> Nothing

      (Tuple (Tuple name isRepeatable) isOptional) <- do
        case optarg of
              (Positional n r') -> pure $ tuple3 n (r' || r) false
              (Command    n r') -> pure $ tuple3 n (r' || r) false
              (Group o (Cons (Cons a Nil) Nil) r) ->
                case a of
                  (Positional n r') -> pure $ tuple3 n (r' || r) o
                  (Command    n r') -> pure $ tuple3 n (r' || r) o
                  otherwise -> Nothing
              otherwise -> Nothing
      pure
        $ Group oo
                (singleton $ singleton (optf name isOptional isRepeatable))
                r
    trySmartOpt x = x

    stdin :: L.TokenParser Argument
    stdin = L.dash *> pure Stdin

    longOption :: L.TokenParser Argument
    longOption = Option <$> do
      { name: name, arg: arg } <- L.lopt
      O.lopt' name arg <$> repetition

    shortOption :: L.TokenParser Argument
    shortOption = OptionStack <$> do
      { flag: flag, stack: stack, arg: arg } <- L.sopt
      O.sopt' flag stack arg <$> repetition

    option :: L.TokenParser Argument
    option = longOption <|> shortOption

    reference :: L.TokenParser Argument
    reference = Reference <$> L.reference

    positional :: L.TokenParser Argument
    positional = Positional <$> (L.angleName <|> L.shoutName)
                            <*> repetition

    command :: L.TokenParser Argument
    command = Command <$> L.name
                      <*> repetition

    group :: L.TokenParser Argument
    group = defer \_ -> P.choice [ reqGroup , optGroup ]

    optGroup :: L.TokenParser Argument
    optGroup = defer \_ -> Group true
      <$> (P.between
            (indented *> L.lsquare)
            (L.rsquare)
            ((some elem) `P.sepBy1` L.vbar))
      <*> repetition

    reqGroup :: L.TokenParser Argument
    reqGroup = defer \_ -> Group false
      <$> (P.between
            (indented *> L.lparen)
            (L.rparen)
            ((some elem) `P.sepBy1` L.vbar))
      <*> repetition

    repetition :: L.TokenParser Boolean
    repetition = P.choice [ P.try $ indented *> L.tripleDot $> true
                          , pure false
                          ]

    program :: L.TokenParser String
    program = "Program name" <??> L.name

parse :: Boolean                  -- ^ Enable "smart-options"
      -> (List L.PositionedToken) -- ^ The token stream
      -> Either P.ParseError (List Usage)
parse smartOpts = flip L.runTokenParser (usageParser smartOpts)

run :: String  -- ^ The usage section text
    -> Boolean -- ^ Enable "smart-options"
    -> Either P.ParseError (List Usage)
run x smartOpts = parse smartOpts =<< L.lexUsage x
