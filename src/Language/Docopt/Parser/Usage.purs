module Language.Docopt.Parser.Usage (
    usageParser
  , run
  , parse
  , module U
  ) where

import Prelude
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.List (List(..), many, some, singleton, length, modifyAt)
import Text.Parsing.Parser (ParseError) as P
import Text.Parsing.Parser.Combinators (try, optional, choice, sepBy1, between,
                                       lookAhead) as P
import Text.Parsing.Parser.Combinators ((<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(Position)) as P
import Data.Either (Either())
import Data.Maybe (Maybe(..), maybe)
import Control.Bind ((=<<))

import Language.Docopt.Parser.Common (markIndent', markLine, indented,
                                     sameIndent, lessIndented, moreIndented)
import Language.Docopt.Parser.Usage.Usage (Usage(..))
import Language.Docopt.Parser.Usage.Argument (Argument(..))
import Language.Docopt.Parser.Usage.Usage as U
import Language.Docopt.Parser.Lexer as L
import Language.Docopt.Parser.Usage.Option as O

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
usageParser :: L.TokenParser (List Usage)
usageParser = do

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
            P.try do
              program
              usageLine name

  where

    usageLine :: String -> L.TokenParser Usage
    usageLine name = Usage name <$> do
      xs <- "Option, Positional, Command, Group or Reference elements" <??> do
                  (many $ P.try $ moreIndented *> elem) `P.sepBy1` L.vbar
      eoa <- P.choice [
        P.try $ do
          moreIndented *> L.doubleDash
          return $ Just EOA
      , (do
          L.eof <|> (P.lookAhead $ lessIndented <|> sameIndent)
          return Nothing
        )
        -- XXX: We could show the last token that failed to be consumed, here
        <?> "start of next usage line or end of usage section"
      ]

      -- Push the EOA onto the last branch (the most right branch)
      return $ maybe xs id do
        e <- eoa
        modifyAt
          (length xs - 1)
          (\as -> as ++ (singleton e))
          xs

    elem :: L.TokenParser Argument
    elem = defer \_ -> do
      P.choice $ P.try <$>
        [ option
        , positional
        , command
        , group
        , reference
        , stdin
        ] <?> "Option, Positional, Command, Group or Reference"

    stdin :: L.TokenParser Argument
    stdin = L.dash *> return Stdin

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
    positional = Positional
      <$> (L.angleName <|> L.shoutName)
      <*> repetition

    command :: L.TokenParser Argument
    command = Command
      <$> L.name
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
    repetition = P.choice
      [ P.try $ indented *> L.tripleDot *> return true
      , return false ]

    program :: L.TokenParser String
    program = "Program name" <??> L.name

parse :: (List L.PositionedToken) -> Either P.ParseError (List Usage)
parse = flip L.runTokenParser usageParser

run :: String -> Either P.ParseError (List Usage)
run x = parse =<< L.lexUsage x
