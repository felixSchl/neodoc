-- | The neodoc usage section parser

module Neodoc.Spec.Parser.Usage where

import Prelude
import Debug.Trace
import Data.NonEmpty (NonEmpty, (:|))
import Neodoc.Data.Layout
import Neodoc.Data.UsageLayout
import Data.Functor (($>))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Bind ((=<<))
import Control.Lazy (defer)
import Control.MonadPlus (guard)
import Data.Either (Either)
import Data.List (
  List(..), many, some, singleton, length, modifyAt, (:), fromFoldable)
import Data.List.Partial (head, tail) as PartialList
import Data.Maybe (fromMaybe, Maybe(..), maybe, isNothing)
import Data.Tuple (Tuple(Tuple), snd, fst)
import Data.Tuple.Nested (tuple3)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError, fatal, fail) as P
import Text.Parsing.Parser.Combinators (
  try, optional, choice, sepBy1, between,
  optionMaybe, lookAhead, option
  ) as P
import Text.Parsing.Parser.Combinators ((<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(Position)) as P

import Neodoc.Spec.Parser.Base (getInput)
import Neodoc.Spec.Lexer as L
import Neodoc.Spec.Parser.Combinators (
  markIndent', markLine, indented,
  sameIndent, lessIndented, moreIndented
)

type Branch = NonEmpty List UsageLayout
type UsageParseResult = {
  program :: String
, layouts :: NonEmpty List (List Branch)
}

parse
  :: List L.PositionedToken
  -> Either P.ParseError UsageParseResult
parse = flip L.runTokenParser do
  -- Calculate and mark the original program indentation.
  P.Position _ startCol <- L.nextTokPos <?> "Program name"
  name    <- program
  layouts <- markLine do
    markIndent' startCol $ do
     (:|)
      <$> (layout name)
      <*> many do
            P.optional $ P.try do
              L.name >>= guard <<< (_ == "or")
              L.colon
            name' <- program
            if name /= name'
               then P.fatal
                      $ "Program name mismatch: Expected \"" <> name <> "\""
                          <> ", but got \"" <> name' <> "\""
               else layout name

  L.eof <?> "End of usage section"
  pure {
    program: name
  , layouts:
      layouts <#> case _ of
        Just (Group _ _ xs) -> fromFoldable $ xs
        Just (x@(Elem _))   -> singleton $ x :| Nil
        _                   -> Nil
  }

  where
  program :: L.TokenParser String
  program = "Program name" <??> L.name

  layout :: String -> L.TokenParser (Maybe UsageLayout)
  layout name = do
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
        pure $ singleton $ singleton $ Elem EOA
    , (do
        L.eof <|> (P.lookAhead $ lessIndented <|> sameIndent)
        pure Nil
      )
      -- XXX: We could show the last token that failed to be consumed, here
      <?> "End of usage line"
    ]

    pure $ case xs <> eoa of
      Nil -> Nothing
      xs  -> pure do
        Group
          false -- not optional
          false -- not repeatable
          (unsafePartial $ listToNonEmpty $ listToNonEmpty <$> xs)

  command :: L.TokenParser UsageLayoutArg
  command = Command
    <$> L.name
    <*> repetition

  positional :: L.TokenParser UsageLayoutArg
  positional = Positional
    <$> (L.shoutName <|> L.angleName)
    <*> repetition

  stdin :: L.TokenParser UsageLayoutArg
  stdin = L.dash *> pure Stdin

  reference :: L.TokenParser UsageLayoutArg
  reference = Reference <$> L.reference

  longOption :: L.TokenParser UsageLayoutArg
  longOption = do
    { name, arg } <- L.lopt
    let arg' = do
          { name, optional } <- arg
          pure (OptionArgument name optional)
    Option name arg' <$> repetition

  shortOption :: L.TokenParser UsageLayoutArg
  shortOption = do
    { chars, arg } <- L.sopt
    let arg' = do
          { name, optional } <- arg
          pure (OptionArgument name optional)
    OptionStack chars arg' <$> repetition

  option :: L.TokenParser UsageLayoutArg
  option = longOption <|> shortOption

  repetition :: L.TokenParser Boolean
  repetition = P.option false (indented *> L.tripleDot $> true)

  elem :: L.TokenParser UsageLayout
  elem = "Option, Positional, Command, Group or Reference" <??>
    (P.choice
      [ Elem <$> positional
      , Elem <$> command
      , Elem <$> reference
      , Elem <$> stdin
      , Elem <$> option
      , defer \_ -> group
      ])

  group :: L.TokenParser UsageLayout
  group = defer \_ -> P.choice [ reqGroup , optGroup ]

  listToNonEmpty :: ∀ a. Partial => List a -> NonEmpty List a
  listToNonEmpty (a : as) = a :| as

  optGroup :: L.TokenParser UsageLayout
  optGroup = defer \_ -> do
    branches <- P.between
                  (indented *> L.lsquare)
                  (L.rsquare)
                  ((some elem) `P.sepBy1` L.vbar)

    Group true
        <$> repetition
        -- note: this can be unsafe because of `sepBy1` and `some`
        <*> (pure $ unsafePartial
                $ listToNonEmpty $ listToNonEmpty <$> branches)

  reqGroup :: L.TokenParser UsageLayout
  reqGroup = defer \_ -> do
    branches <- P.between
                  (indented *> L.lparen)
                  (L.rparen)
                  ((some elem) `P.sepBy1` L.vbar)

    Group false
        <$> repetition
        -- note: this can be unsafe because of `sepBy1` and `some`
        <*> (pure $ unsafePartial
                $ listToNonEmpty $ listToNonEmpty <$> branches)

  maybeInParens :: ∀ a. L.TokenParser a -> L.TokenParser a
  maybeInParens p = do
    Tuple close v <- moreIndented *> do
      Tuple
        <$> (P.optionMaybe $ P.choice [ L.lparen  $> L.rparen
                                      , L.lsquare $> L.rsquare ])
        <*> p
    fromMaybe (pure unit) close
    pure v
