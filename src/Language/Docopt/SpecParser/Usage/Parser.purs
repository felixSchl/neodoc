module Language.Docopt.SpecParser.Usage.Parser (
    usageParser
  , parse
  ) where

import Prelude
import Debug.Trace
import Debug.Profile
import Data.Functor (($>))
import Data.NonEmpty (NonEmpty, (:|))
import Language.Docopt.SpecParser.Lexer as L
import Language.Docopt.SpecParser.Usage.Option as O
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Bind ((=<<))
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Data.Either (Either)
import Data.List (List(..), many, some, singleton, length, modifyAt, (:))
import Data.Maybe (fromMaybe, Maybe(..), maybe, isNothing)
import Data.Tuple (Tuple(Tuple), snd, fst)
import Data.Tuple.Nested (tuple3)
import Language.Docopt.SpecParser.Common (markIndent', markLine, indented,
                                     sameIndent, lessIndented, moreIndented)
import Language.Docopt.SpecParser.Usage.Argument (Argument(..))
import Language.Docopt.SpecParser.Usage.Usage (Usage(..))
import Text.Parsing.Parser (ParseError, fatal) as P
import Text.Parsing.Parser.Combinators (try, optional, choice, sepBy1, between,
                                       optionMaybe, lookAhead, option) as P
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
usageParser
  :: Boolean -- ^ Enable "smart-options" parsing
  -> L.TokenParser {
      program :: String
    , usages  :: List Usage
    }
usageParser smartOpts = do

  -- Calculate and mark the original program indentation.
  P.Position _ startCol <- L.nextTokPos <?> "Program name"
  name   <- program
  usages <- markLine do
    markIndent' startCol $ do
     (:)
      <$> (usageLine name)
      <*> many do
            P.optional $ P.try do
              L.name >>= guard <<< (_ == "or")
              L.colon
            name' <- program
            if name /= name'
               then P.fatal
                      $ "Program name mismatch: Expected \"" <> name <> "\""
                          <> ", but got \"" <> name' <> "\""
               else usageLine name

  L.eof <?> "End of usage section"
  pure {
    program: name
  , usages:  usages
  }

  where

    usageLine :: String -> L.TokenParser Usage
    usageLine name = do
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
          <$> (P.optionMaybe $ P.choice [ L.lparen  $> L.rparen
                                        , L.lsquare $> L.rsquare ])
          <*> p
      fromMaybe (pure unit) close
      pure v

    elem :: L.TokenParser Argument
    elem = "Option, Positional, Command, Group or Reference" <??>
      (P.choice
        [ positional
        , command
        , reference
        , stdin
        , option
        , defer \_ -> (if smartOpts then trySmartOpt else id) <$> group
        ])

    trySmartOpt :: Argument -> Argument
    trySmartOpt grp'@(Group grp) = fromMaybe grp' $ do
      Tuple opt optarg <- case grp.branches of
                              ((opt':(arg':Nil)):Nil) ->
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
              (Positional pos) ->
                pure $ tuple3 pos.name
                              (pos.repeatable)
                              false
              (Command cmd) ->
                pure $ tuple3 cmd.name
                              (cmd.repeatable)
                              false
              (Group (grp''@{ branches: (a:Nil):Nil })) ->
                case a of
                  (Positional pos) ->
                    pure $ tuple3 pos.name
                                  (pos.repeatable || grp''.repeatable)
                                  grp''.optional
                  (Command cmd) ->
                    pure $ tuple3 cmd.name
                                  (cmd.repeatable || grp''.repeatable)
                                  grp''.optional
                  otherwise -> Nothing
              otherwise -> Nothing

      pure $ Group grp {
        branches = singleton $ singleton (optf name isOptional isRepeatable)
      }
    trySmartOpt x = x

    stdin :: L.TokenParser Argument
    stdin = L.dash *> pure Stdin

    longOption :: L.TokenParser Argument
    longOption = Option <$> do
      { name, arg } <- L.lopt
      r <- repetition
      pure  { name:       name
            , arg:        arg
            , repeatable: r
            }

    shortOption :: L.TokenParser Argument
    shortOption = OptionStack <$> do
      { chars: (flag :| stack), arg } <- L.sopt
      r <- repetition
      pure  { flag:       flag
            , stack:      stack
            , arg:        arg
            , repeatable: r
            }

    option :: L.TokenParser Argument
    option = longOption <|> shortOption

    reference :: L.TokenParser Argument
    reference = Reference <$> L.reference

    positional :: L.TokenParser Argument
    positional = Positional <$> do
      name <- L.shoutName <|> L.angleName
      r    <- repetition
      pure { name: name, repeatable: r }

    command :: L.TokenParser Argument
    command = Command <$> do
      name <-  L.name
      r    <- repetition
      pure { name: name, repeatable: r }

    group :: L.TokenParser Argument
    group = defer \_ -> P.choice [ reqGroup , optGroup ]

    optGroup :: L.TokenParser Argument
    optGroup = defer \_ -> Group <$> do
      branches <- P.between
                    (indented *> L.lsquare)
                    (L.rsquare)
                    ((some elem) `P.sepBy1` L.vbar)
      r <- repetition
      pure { optional: true, branches: branches, repeatable: r }

    reqGroup :: L.TokenParser Argument
    reqGroup = defer \_ -> Group <$> do
      branches <- P.between
                    (indented *> L.lparen)
                    (L.rparen)
                    ((some elem) `P.sepBy1` L.vbar)
      r <- repetition
      pure { optional: false, branches: branches, repeatable: r }

    repetition :: L.TokenParser Boolean
    repetition = P.option false (indented *> L.tripleDot $> true)

    program :: L.TokenParser String
    program = "Program name" <??> L.name

parse
  :: Boolean                  -- ^ Enable "smart-options"
  -> (List L.PositionedToken) -- ^ The token stream
  -> Either P.ParseError {
      program :: String
    , usages  :: List Usage
    }
parse smartOpts = flip L.runTokenParser (usageParser smartOpts)
