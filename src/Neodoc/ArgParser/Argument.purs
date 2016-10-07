module Neodoc.ArgParser.Argument where

import Prelude
import Debug.Trace
import Data.List.Partial as LU
import Data.Array as A
import Data.Array.Partial as AU
import Control.Plus (empty)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (
  List(..), reverse, singleton, concat, length, (:)
, some, filter, head, toUnfoldable, sortBy, groupBy, last, null
, tail, many, mapWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing, isJust)
import Text.Parsing.Parser (
  PState(..), ParseError(..), ParserT(..), Result(..), fail
, parseFailedFatal, parseFailed, unParserT, fatal) as P
import Text.Parsing.Parser.Combinators (
  option, try, lookAhead, (<?>), choice) as P
import Text.Parsing.Parser.Pos (Position(..)) as P
import Data.String.Ext ((~~))
import Data.String as String
import Data.Pretty (pretty)
import Data.String (fromCharArray, stripPrefix)
import Partial.Unsafe (unsafePartial)

import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Token
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Value as Value
import Neodoc.Value (Value(..))
import Neodoc.Data.OptionArgument (OptionArgument(..), isOptionArgumentOptional)
import Neodoc.Data.SolvedLayout (SolvedLayoutArg)

command :: ∀ r. String -> String -> ArgParser r Value
command rep n = token rep case _ of
  Lit s | s == n -> Just (BoolValue true)
  _              -> Nothing

positional :: ∀ r. String -> String -> ArgParser r Value
positional rep n = token rep case _ of
  Lit v -> Just (Value.read v false)
  _     -> Nothing

optionArgument :: ∀ r. ArgParser r Value
optionArgument = token "option-argument" case _ of
  Lit v -> Just (Value.read v false)
  _     -> Nothing

eoa :: ∀ r. ArgParser r Value
eoa = token "--" case _ of
  EOA xs -> Just (ArrayValue (toUnfoldable xs))
  _      -> Nothing

stdin :: ∀ r. ArgParser r Value
stdin = token "-" case _ of
  Stdin -> Just (BoolValue true)
  _     -> Nothing

token :: ∀ r a. String -> (Token -> Maybe a) -> ArgParser r a
token name test = Parser \c s g i ->
  let _return = Step true c s g
      _fail m = Step false c s g i (Left $ ParseError false (Left m))
   in case i of
    (PositionedToken token source _) : ss ->
      case test token of
        Nothing -> _fail $ "expected " <> name <> ", but got: " <> source
        Just a  -> _return ss (Right a)
    _ -> _fail $ "expected " <> name

type CanRepeat = Boolean
type CanTerminate = Boolean
data OptRes = OptRes Value CanTerminate CanRepeat

longOption
  :: ∀ r
   . Boolean -- ^ does this option attempt to terminate the parse?
  -> String  -- ^ name of the option w/o leading dashes, i.e.: '--foo' => "foo"
  -> (Maybe OptionArgument)
  -> ArgParser r OptRes
longOption term n mArg = do
  input <- getInput
  case input of
    (PositionedToken token _ _):xs ->
      let nextToken = getToken <$> head xs
       in do
        result <- go token nextToken

        -- update the parser input
        setInput case xs of
          _:xs' | result.hasConsumedArg -> xs'
          _ -> xs

        pure if term
          then
            -- remove the consumed arg from the input, if necessary
            let val = ArrayValue $ maybe [] (A.singleton <<< StringValue) result.rawValue
             in OptRes val true false
          else
            -- remove the consumed arg from the input, if necessary
            let val = maybe (BoolValue true) (flip Value.read false) result.rawValue
             in OptRes val true $ not result.explicitArg

    _ -> fail "Expected token, met EOF" -- XXX: improve message

  where
  isFlag = isNothing mArg

  -- case 0:
  -- The name is an exact match and found in 'options.stopAt'.
  -- Note that the option may *NOT* have an explicitly assigned
  -- option-argument. Finally, let the caller do the actual termination
  -- (updating parser state / consuming all input)
  go (LOpt n' Nothing) _ | (n' == n) && term
    = pure { rawValue:       Nothing
           , hasConsumedArg: false
           , explicitArg:    false
           }

  -- case 1:
  -- The name is an exact match
  go (LOpt n' v) atok | (not isFlag) && (n' == n)
    = case v of
        Just s ->
          pure  { rawValue:       Just s
                , hasConsumedArg: false
                , explicitArg:    true
                }
        _  -> case atok of
          Just (Lit s) ->
            pure  { rawValue:       Just s
                  , hasConsumedArg: true
                  , explicitArg:    false
                  }
          _ ->
            let argIsOptional = maybe true isOptionArgumentOptional mArg
             in if term || argIsOptional
                then pure { rawValue:       Nothing
                          , hasConsumedArg: false
                          , explicitArg:    false
                          }
                else fatal' $ optionRequiresArgumentError (OptionAlias.Long n)

  -- case 2:
  -- The name is an exact match and takes no argument
  go (LOpt n' v) _ | isFlag && (n' == n)
    = case v of
        Just _  -> fatal' $ optionTakesNoArgumentError (OptionAlias.Long n)
        Nothing -> pure { rawValue:       Nothing
                        , hasConsumedArg: false
                        , explicitArg:    false
                        }

  -- case 3:
  -- The name is a substring of the input and no explicit argument has been
  -- provdided.
  go (LOpt n' Nothing) _ | not isFlag
    = case stripPrefix n n' of
        Just s ->
          pure { rawValue:        Just s
                , hasConsumedArg: false
                , explicitArg:    false
                }
        _ -> fail "Invalid substring"

  go a _ = fail $ "Invalid token: " <> pretty a

shortOption
  :: ∀ r
   . Boolean -- ^ does this option attempt to terminate the parse?
  -> Char    -- ^ flag of the option w/o leading dash, i.e.: '-f' => "f"
  -> (Maybe OptionArgument)
  -> ArgParser r OptRes
shortOption term f mArg = do
  input <- getInput
  case input of
    (PositionedToken token source sourcePos):xs ->
      let nextToken = getToken <$> head xs
       in do
        result <- go token nextToken
        if term && isNothing result.remainder
          then
            let
              -- note: recover the argument in it's string form.
              -- the `drop 2` drops the leading dash and the first character
              -- from the input. this makes the assumption that the source will
              -- *always* start with a dash and then a character.
              v = maybe [] (const [ StringValue $ String.drop 2 source ]) result.remainder
              va = maybe [] (A.singleton <<< StringValue) result.rawValue
              val = ArrayValue $ v <> va
              newInput = case xs of
                            _:xs' | result.hasConsumedArg -> xs'
                            _ -> xs
             in setInput newInput $> do
                  OptRes val true false
          else
            let newSource = "-" <> String.drop 2 source
                pushed = maybe empty
                          (\tok -> singleton $ PositionedToken tok newSource sourcePos)
                          result.remainder
                rest = case xs of
                        _:xs' | result.hasConsumedArg -> xs'
                        _ -> xs
                -- shift the positions of the artificial tokens by the number
                -- of pushed tokens (which is always 1, but we check for
                -- completeness sake).
                nPushed = length pushed
                rest' = if nPushed > 0
                          then rest <#> \(PositionedToken tok source sourcePos) ->
                                PositionedToken tok source (sourcePos + nPushed)
                          else rest
                val = maybe (BoolValue true) (flip Value.read false) result.rawValue
                newInput = pushed <> rest'
             in setInput newInput $> do
                  OptRes val false $ not result.explicitArg

    _ -> fail "Expected token, met EOF" -- XXX: improve message

  where
  isFlag = isNothing mArg

  -- case 1:
  -- The leading flag matches, there are no stacked options, and an explicit
  -- argument may have been passed.
  go (SOpt f' xs v) atok | (f' == f) && (not isFlag) && (A.null xs)
    = case v of
        Just s ->
          pure { rawValue:       Just s
                , remainder:      Nothing
                , hasConsumedArg: false
                , explicitArg:    true
                }
        otherwise -> case atok of
          Just (Lit s) ->
            pure { rawValue:       Just s
                  , remainder:      Nothing
                  , hasConsumedArg: true
                  , explicitArg:    false
                  }
          otherwise ->
            let argIsOptional = maybe true isOptionArgumentOptional mArg
             in if term || argIsOptional
                then pure { rawValue:       Nothing
                          , remainder:      Nothing
                          , hasConsumedArg: false
                          , explicitArg:    false
                          }
                else fatal' $ optionRequiresArgumentError (OptionAlias.Short f)

  -- case 2:
  -- The leading flag matches, there are stacked options, a explicit
  -- argument may have been passed and the option takes an argument.
  go (SOpt f' xs v) _ | (f' == f) && (not isFlag) && (not $ A.null xs)
    -- note: we put the '=' back on. this assumes that explicit arguments will
    --       *always* be bound with a '='.
    = let arg = fromCharArray xs <> maybe "" ("=" <> _) v
       in pure  { rawValue:       Just arg
                , remainder:      Nothing
                , hasConsumedArg: false
                , explicitArg:    isJust v
                }

  -- case 3:
  -- The leading flag matches, there are stacked options, the option takes
  -- no argument and an explicit argument has not been provided.
  go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (not $ A.null xs)
    = pure  { rawValue:       Nothing
            , hasConsumedArg: false
            , explicitArg:    isJust v
            , remainder:      pure (SOpt (unsafePartial $ AU.head xs)
                                        (unsafePartial $ AU.tail xs)
                                        v)
            }

  -- case 4:
  -- The leading flag matches, there are no stacked options and the option
  -- takes no argument - total consumption!
  go (SOpt f' xs v) _ | (f' == f) && (isFlag) && (A.null xs)
    = case v of
        Just _  -> fatal' $ optionTakesNoArgumentError (OptionAlias.Short f')
        Nothing -> pure { rawValue:       Nothing
                        , remainder:      Nothing
                        , hasConsumedArg: false
                        , explicitArg:    false
                        }

  go a _ = fail $ "Invalid token: " <> pretty a
