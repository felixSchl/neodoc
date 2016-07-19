module Language.Docopt.ArgParser.Parser.Atom where

import Prelude
import Data.List.Partial as LU
import Data.Array as A
import Data.Array.Partial as AU
import Control.Plus (empty)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (List(..), reverse, singleton, concat, length, (:),
                  some, filter, head, toUnfoldable, sortBy, groupBy, last, null,
                  tail, many, mapWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Text.Parsing.Parser (PState(..), ParseError(..), ParserT(..), Result(..), fail,
                            parseFailedFatal, parseFailed, unParserT, fatal) as P
import Text.Parsing.Parser.Combinators (option, try, lookAhead, (<?>), choice) as P
import Text.Parsing.Parser.Pos (Position(..)) as P
import Data.String.Ext ((~~))
import Data.String as String
import Data.String (fromCharArray, stripPrefix)
import Partial.Unsafe (unsafePartial)
import Language.Docopt.Value as Value
import Language.Docopt.Value (Value(..))
import Language.Docopt.ArgParser.Parser.Types
import Language.Docopt.Argument (Argument(..), Branch, OptionArgumentObj()) as D
import Language.Docopt.ArgParser.Token (PositionedToken(..), Token(..),
                                        unPositionedToken)
import Language.Docopt.ArgParser.Parser.Token

eoa :: Parser Value
eoa = token go P.<?> "--"
  where
    go (EOA xs) = Just (ArrayValue (toUnfoldable xs))
    go _        = Nothing

command :: String -> Parser Value
command n = token go P.<?> "command " ~~ show n
  where
    go (Lit s) | s == n = Just (BoolValue true)
    go _                = Nothing

positional :: String -> Parser Value
positional n = token go P.<?> "positional argument " ~~ show n
  where
    go (Lit v) = Just (Value.read v false)
    go _       = Nothing

optionArgument :: Parser Value
optionArgument = token go P.<?> "option-argument"
  where
    go (Lit v) = Just (Value.read v false)
    go _       = Nothing

stdin :: Parser Value
stdin = token go P.<?> "-"
  where
    go Stdin = Just (BoolValue true)
    go _     = Nothing

longOption :: Boolean -> String -> (Maybe D.OptionArgumentObj) -> Parser Value
longOption term n a = unsafePartial $
 P.ParserT $ \(P.PState toks pos) ->
  pure $ case toks of
    Cons (PositionedToken { token: tok, sourcePos: npos, source: s }) xs ->
      case go tok (_.token <<< unPositionedToken <$> head xs) of
        Left e -> P.parseFailed toks npos e
        Right result -> do
          let value = fromMaybe (BoolValue true) do
                                rawValue <- result.rawValue
                                pure (Value.read rawValue false)
          if term
            then
              -- consume the rest of the source of this short option into a
              -- single value.
              P.Result
                (if result.hasConsumedArg
                      then unsafePartial $ LU.tail xs
                      else xs)
                (let
                    -- recover the argument in it's string form.
                    va = maybe [] (A.singleton <<< StringValue) result.rawValue
                  in Right (ArrayValue va))
                true
                pos
            else
              P.Result
                (let
                    pushed
                      = maybe empty
                          (\v' -> singleton $ PositionedToken {
                                    token:     v'
                                  , sourcePos: pos
                                  , source:    s
                                  })
                          result.remainder
                    rest = if result.hasConsumedArg then LU.tail xs else xs
                  in pushed <> rest)
                (Right value)
                (maybe true (const false) result.remainder)
                (maybe pos (_.sourcePos <<< unPositionedToken) (head xs))
    _ -> P.parseFailed toks pos "Expected token, met EOF"

  where
    isFlag = isNothing a

    -- case 0:
    -- The name is an exact match and found in 'options.stopAt'.
    -- Note that the option may *NOT* have an explicitly assigned
    -- option-argument. Finally, let the caller do the actual termination
    -- (updating parser state / consuming all input)
    go (LOpt n' Nothing) _ | (n' == n) && term
      = pure { rawValue:       Nothing
             , remainder:      Nothing
             , hasConsumedArg: false
             }

    -- case 1:
    -- The name is an exact match
    go (LOpt n' v) atok | (not isFlag) && (n' == n)
      = case v of
          Just s ->
            pure { rawValue:       Just s
                 , remainder:      Nothing
                 , hasConsumedArg: false
                 }
          _  -> case atok of
            Just (Lit s) ->
              pure { rawValue:       Just s
                   , remainder:      Nothing
                   , hasConsumedArg: true
                   }
            otherwise    ->
              if term || (fromMaybe true (_.optional <$> a))
                 then pure { rawValue:       Nothing
                           , remainder:      Nothing
                           , hasConsumedArg: false
                           }
                 else Left $ "Option requires argument: --" <> n'

    -- case 2:
    -- The name is an exact match and takes no argument
    go (LOpt n' v) _ | isFlag && (n' == n)
      = case v of
             Just _  -> Left $ "Option takes no argument: --" <> n'
             Nothing -> pure { rawValue:       Nothing
                             , remainder:      Nothing
                             , hasConsumedArg: false
                             }

    -- case 3:
    -- The name is a substring of the input and no explicit argument has been
    -- provdided.
    go (LOpt n' Nothing) _ | not isFlag
      = case stripPrefix n n' of
          Just s ->
            pure { rawValue:       Just s
                 , remainder:      Nothing
                 , hasConsumedArg: false
                 }
          otherwise -> Left "Invalid substring"

    go a b = Left $ "Invalid token: " <> show a <> " (input: " <> show b <> ")"

shortOption
  :: Boolean
  -> Char
  -> (Maybe D.OptionArgumentObj)
  -> Parser (Tuple Value Boolean)
shortOption term f a = unsafePartial $
 P.ParserT $ \(P.PState toks pos) -> do
  pure $ case toks of
    Cons (PositionedToken { token: tok, source: s }) xs ->
      case go tok (_.token <<< unPositionedToken <$> head xs) of
        Left e -> P.parseFailed toks pos e
        Right result -> do
          let value = fromMaybe (BoolValue true) do
                                rawValue <- result.rawValue
                                pure (Value.read rawValue false)
          if term && isNothing result.remainder
            then
              -- consume the rest of the source of this short option into a
              -- single value.
              P.Result
                (if result.hasConsumedArg then LU.tail xs else xs)
                (let
                    -- recover the argument in it's string form.
                    v = maybe [] (\_ -> [ StringValue $ String.drop 2 s ]) result.remainder
                    va = maybe [] (A.singleton <<< StringValue) result.rawValue
                  in Right (Tuple (ArrayValue $ v <> va) true))
                true
                pos
            else
              P.Result
                (let
                  pushed = maybe empty
                                  (\v' ->
                                    singleton
                                      $ PositionedToken
                                          { token:     v'
                                          , sourcePos: pos
                                          , source:    "-" <> String.drop 2 s
                                          })
                                  result.remainder
                  rest = if result.hasConsumedArg then LU.tail xs else xs
                  rest' = rest <#> \(PositionedToken p) ->
                            PositionedToken $ p {
                              sourcePos = case p.sourcePos of
                                P.Position n x -> P.Position (n + 1) x
                            }
                in pushed <> rest')
                (Right (Tuple value false))
                (maybe true (const false) result.remainder)
                (maybe pos (_.sourcePos <<< unPositionedToken) (head xs))
    _ -> P.parseFailed toks pos "Expected token, met EOF"

  where
    isFlag = isNothing a

    -- case 1:
    -- The leading flag matches, there are no stacked options, and an explicit
    -- argument may have been passed.
    go (SOpt f' xs v) atok | (f' == f) && (not isFlag) && (A.null xs)
      = case v of
          Just s ->
            pure { rawValue:       Just s
                 , remainder:      Nothing
                 , hasConsumedArg: false
                 }
          otherwise -> case atok of
            Just (Lit s) ->
              pure { rawValue:       Just s
                   , remainder:      Nothing
                   , hasConsumedArg: true
                   }
            otherwise ->
              if term || (fromMaybe true (_.optional <$> a))
                 then pure { rawValue:       Nothing
                           , remainder:      Nothing
                           , hasConsumedArg: false
                           }
                 else  Left $ "Option requires argument: -" <> String.singleton f'

    -- case 2:
    -- The leading flag matches, there are stacked options, a explicit
    -- argument may have been passed and the option takes an argument.
    go (SOpt f' xs v) _ | (f' == f) && (not isFlag) && (not $ A.null xs)
      = do
        let arg = fromCharArray xs <> maybe "" ("=" <> _) v
        pure { rawValue:       Just arg
             , remainder:      Nothing
             , hasConsumedArg: false
             }

    -- case 3:
    -- The leading flag matches, there are stacked options, the option takes
    -- no argument and an explicit argument has not been provided.
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (not $ A.null xs)
      = pure { rawValue:       Nothing
             , remainder:      pure (SOpt (unsafePartial $ AU.head xs)
                                          (unsafePartial $ AU.tail xs)
                                          v)
             , hasConsumedArg: false
             }

    -- case 4:
    -- The leading flag matches, there are no stacked options and the option
    -- takes no argument - total consumption!
    go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (A.null xs)
      = case v of
              Just _  -> Left $ "Option takes no argument: -" <> String.singleton f'
              Nothing -> pure { rawValue:       Nothing
                              , remainder:      Nothing
                              , hasConsumedArg: false
                              }

    go a b = Left $ "Invalid token: " <> show a <> " (input: " <> show b <> ")"

