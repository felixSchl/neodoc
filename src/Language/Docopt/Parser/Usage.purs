module Language.Docopt.Parser.Usage (
    usageParser
  , run
  , parse
  , module U
  ) where

import Prelude
import Debug.Trace
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Control.Monad.Trans (lift)
import Control.Monad.State (get)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Foldable (intercalate)
import Data.List (List(..), many, some, (:), toList, concat, singleton
                  , modifyAt, length)
import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P
import Data.List as L
import Data.String (fromChar)
import Data.String as Str
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Array as A
import Control.Bind ((=<<))

import Language.Docopt.Parser.Base
import Language.Docopt.Parser.Common
import Language.Docopt.Parser.State
import Language.Docopt.Parser.Usage.Usage
import Language.Docopt.Parser.Usage.Argument
import Language.Docopt.Parser.Usage.Usage  as U
import Language.Docopt.Parser.Lexer        as L
import Language.Docopt.Parser.Usage.Option as O

-- | L.TokenParser to parse the usage section
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
  name <- program
  col' <- getCol
  let startCol = col' - (Str.length name) - 1
  markIndent' startCol $ do
    Cons
    <$> (usageLine name)
    <*> many do program *> usageLine name

  where

    usageLine :: String -> L.TokenParser Usage
    usageLine name = Usage name <$> do
      xs  <- (many $ moreIndented *> elem) `P.sepBy1` L.vbar
      eoa <- P.choice [
        P.try $ do
          moreIndented *> L.doubleDash
          return $ Just EOA
      , (do
          L.eof <|> (P.lookAhead $ lessIndented <|> sameIndent)
          return Nothing
        )
        -- XXX: We could show the last token that failed to be consumed, here
        P.<?> "start of next usage line or end of usage section"
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
        ] P.<?> "Option, Positional, Command, Group or Reference"

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
    command = do
      cmd <- Command <$> L.name
      P.notFollowedBy L.tripleDot -- Commands may not repeat!
      pure cmd

    group :: L.TokenParser Argument
    group = defer \_ -> P.choice
      [ reqGroup
      , optGroup ]

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
      [ P.try $ indented *> L.tripleDot *> pure true
      , pure false ]

    program :: L.TokenParser String
    program = (L.name <|> L.word) P.<?> "Program Name"

parse :: (List L.PositionedToken) -> Either P.ParseError (List Usage)
parse = flip L.runTokenParser usageParser

run :: String -> Either P.ParseError (List Usage)
run x = parse =<< L.lexUsage x
