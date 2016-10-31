-- | The neodoc usage section parser

module Neodoc.Spec.Parser.Usage where

import Prelude
import Debug.Trace
import Debug.Profile
import Data.Pretty
import Data.Bifunctor (lmap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NonEmpty
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
  List(..), many, some, singleton, length, modifyAt, (:), fromFoldable, filter
, reverse, null)
import Data.List.Partial (head, tail) as PartialList
import Data.Maybe (fromMaybe, Maybe(..), maybe, isNothing)
import Data.Tuple (Tuple(Tuple), snd, fst)
import Data.Tuple.Nested (tuple3)
import Partial.Unsafe (unsafePartial)

-- XXX: remove this
import Text.Parsing.Parser.Pos (Position(Position)) as P

import Neodoc.Data.OptionArgument
import Neodoc.Parsing.Parser as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Spec.Error
import Neodoc.Spec.Token as P
import Neodoc.Spec.TokenParser as P
import Neodoc.Spec.Parser.Combinators as P

type Branch = NonEmpty List UsageLayout
type UsageParseResult = {
  program :: String
, layouts :: NonEmpty List (List Branch)
}

parse
  :: List P.PositionedToken
  -> Either SpecParseError UsageParseResult
parse toks = profileS "spec-parser::parse-usage" \_->
 lmap SpecParseError $ P.runTokenParser toks do
  -- Calculate and mark the original program indentation.
  P.Position { column: startCol } <- P.nextTokPos <?> "Program name"
  name <- program
  layouts <- do
    P.markIndent' startCol $ do
     (:|)
      <$> (layout name)
      <*> P.many' do
            P.optional $ P.try do
              P.name >>= guard <<< (_ == "or")
              P.colon
            name' <- program
            if name /= name'
               then do
                  P.fatal
                      $ "Program name mismatch: "
                          <> "Expected \"" <> name <> "\""
                          <> ", but got \"" <> name' <> "\""
               else layout name

  P.eof <?> "End of usage section"

  P.return {
    program: name
  , layouts:
      layouts <#> case _ of
        Just (Group _ _ xs) -> NonEmpty.toList $ xs
        Just (x@(Elem _))   -> singleton $ x :| Nil
        Nothing             -> Nil
  }

  where
  program :: P.TokenParser String
  program = "Program name" <??> P.name

  layout :: String -> P.TokenParser (Maybe UsageLayout)
  layout name = do
    branches <- "Option, Positional, Command, Group or Reference elements" <??> do
      (P.many' $ P.try $ P.moreIndented *> elem) `P.sepBy1` P.vbar
    eoa <- P.choice [
      P.try $ do
        maybeInParens do
          maybeInParens do
            P.doubleDash
            P.many' elem
          P.many' elem
        P.many' elem
        P.return $ Just $ Elem EOA
    , (do
        P.eof <|> (P.lookAhead $ P.lessIndented <|> P.sameIndent)
        P.return Nothing
      )
      -- XXX: We could show the last token that failed to be consumed, here
      <?> "End of usage line"
    ]

    -- attach the 'eoa' to the last branch
    let branches' = reverse case reverse branches of
          Nil  -> maybe Nil (singleton <<< singleton) eoa
          x:xs -> (x <> (maybe Nil (singleton) eoa)) : xs

        branches'' = filter (not <<< null) branches'

    P.return case branches'' of
      Nil -> Nothing
      _   -> Just do
        Group
          false -- not optional
          false -- not repeatable
          (unsafePartial $ listToNonEmpty $ listToNonEmpty <$> branches'')

  command :: P.TokenParser UsageLayoutArg
  command = Command
    <$> P.name
    <*> repetition

  positional :: P.TokenParser UsageLayoutArg
  positional = Positional
    <$> (P.shoutName <|> P.angleName)
    <*> repetition

  stdin :: P.TokenParser UsageLayoutArg
  stdin = P.dash *> P.return Stdin

  reference :: P.TokenParser UsageLayoutArg
  reference = Reference <$> P.reference

  longOption :: P.TokenParser UsageLayoutArg
  longOption = do
    { name, arg } <- P.lopt
    let arg' = do
          { name, optional } <- arg
          Just (OptionArgument name optional)
    Option name arg' <$> repetition

  shortOption :: P.TokenParser UsageLayoutArg
  shortOption = do
    { chars, arg } <- P.sopt
    let arg' = do
          { name, optional } <- arg
          Just (OptionArgument name optional)
    OptionStack chars arg' <$> repetition

  option :: P.TokenParser UsageLayoutArg
  option = longOption <|> shortOption

  repetition :: P.TokenParser Boolean
  repetition = P.option false (P.indented *> P.tripleDot $> true)

  elem :: P.TokenParser UsageLayout
  elem = "Option, Positional, Command, Group or Reference" <??>
    (P.choice
      [ Elem <$> positional
      , Elem <$> command
      , Elem <$> reference
      , Elem <$> stdin
      , Elem <$> option
      , defer \_ -> group
      ])

  group :: P.TokenParser UsageLayout
  group = defer \_ -> P.choice [ reqGroup , optGroup ]

  listToNonEmpty :: ∀ a. Partial => List a -> NonEmpty List a
  listToNonEmpty (a : as) = a :| as

  optGroup :: P.TokenParser UsageLayout
  optGroup = defer \_ -> do
    branches <- P.between
                  (P.indented *> P.lsquare)
                  (P.rsquare)
                  ((some elem) `P.sepBy1` P.vbar)

    Group true
        <$> repetition
        -- note: this can be unsafe because of `sepBy1` and `some`
        <*> (P.return $ unsafePartial
                      $ listToNonEmpty $ listToNonEmpty <$> branches)

  reqGroup :: P.TokenParser UsageLayout
  reqGroup = defer \_ -> do
    branches <- P.between
                  (P.indented *> P.lparen)
                  (P.rparen)
                  ((some elem) `P.sepBy1` P.vbar)

    Group false
        <$> repetition
        -- note: this can be unsafe because of `sepBy1` and `some`
        <*> (P.return $ unsafePartial
                      $ listToNonEmpty $ listToNonEmpty <$> branches)

  maybeInParens :: ∀ a. P.TokenParser a -> P.TokenParser a
  maybeInParens p = do
    Tuple close v <- P.moreIndented *> do
      Tuple
        <$> (P.optionMaybe $ P.choice [ P.lparen  $> P.rparen
                                      , P.lsquare $> P.rsquare ])
        <*> p
    fromMaybe (P.return unit) close
    P.return v
