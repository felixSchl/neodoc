module Language.Docopt.Value (
    Value (..)
  , read
  , parse
  , isSameValueType
  , isBoolValue
  , isArrayValue
  , prettyPrintValue
  , intoArray
  ) where

import Prelude
import Data.Generic (class Generic)
import Data.Either (Either(), either)
import Data.Maybe (fromJust)
import Data.List (List(..), toUnfoldable, many, some)
import Data.Foldable (foldMap)
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Text.Parsing.Parser (ParseError, runParser) as P
import Text.Parsing.Parser.Combinators (between, choice, try, sepBy1, option) as P
import Text.Parsing.Parser.String (noneOf, char, string, eof, satisfy) as P
import Language.Docopt.SpecParser.Base (digit)
import Data.Array as A
import Data.Int (toNumber, fromString) as Int
import Data.String (singleton) as String
import Partial.Unsafe (unsafePartial)
import Global (readFloat)

data Value
  = StringValue String
  | BoolValue   Boolean
  | ArrayValue  (Array Value)
  | IntValue    Int
  | FloatValue  Number

derive instance genericValue :: Generic Value

instance showValue :: Show Value where
  show (StringValue s) = "StringValue " <> s
  show (BoolValue   b) = "BoolValue "   <> show b
  show (ArrayValue xs) = "ArrayValue "  <> show (show <$> xs)
  show (IntValue    x) = "IntValue "    <> show x
  show (FloatValue  x) = "FloatValue "  <> show x

instance eqValue :: Eq Value where
  eq (StringValue s) (StringValue s') = s  == s'
  eq (BoolValue   b) (BoolValue   b') = b  == b'
  eq (ArrayValue xs) (ArrayValue xs') = xs == xs'
  eq (FloatValue  x) (FloatValue  x') = x  == x'
  eq (IntValue    x) (IntValue    x') = x  == x'
  eq _               _                = false

isSameValueType :: Value -> Value -> Boolean
isSameValueType (StringValue _) (StringValue _) = true
isSameValueType (BoolValue   _) (BoolValue   _) = true
isSameValueType (ArrayValue  _) (ArrayValue  _) = true
isSameValueType (IntValue    _) (IntValue    _) = true
isSameValueType (FloatValue  _) (FloatValue  _) = true
isSameValueType _               _               = false

isBoolValue :: Value -> Boolean
isBoolValue (BoolValue _) = true
isBoolValue _             = false

isArrayValue :: Value -> Boolean
isArrayValue (ArrayValue _) = true
isArrayValue _              = false

isIntValue :: Value -> Boolean
isIntValue (IntValue _) = true
isIntValue _            = false

isFloatValue :: Value -> Boolean
isFloatValue (FloatValue _) = true
isFloatValue _              = false

prettyPrintValue :: Value -> String
prettyPrintValue (StringValue s) = s
prettyPrintValue (BoolValue   b) = show b
prettyPrintValue (ArrayValue xs) = show $ prettyPrintValue <$> xs
prettyPrintValue (IntValue    i) = show i
prettyPrintValue (FloatValue  f) = show f

intoArray (ArrayValue xs) = xs
intoArray v               = [v]

read :: String  -- ^ the input
     -> Boolean -- ^ allow splitting?
     -> Value
read s split = either (const $ StringValue s) id (parse s split)

-- | Parse a string into a value
-- | Values can be comma *AND* space separated:
-- |
-- | a  b  c -> [ a, b, c ]
-- | a, b, c -> [ a, b, c ]
-- | a  b, c -> [ a, b, c ]
-- | a, b  c -> [ a, b, c ]
-- |
parse :: String  -- ^ the input
      -> Boolean -- ^ allow splitting?
      -> Either P.ParseError Value
parse s split = P.runParser s $ if split then values else value <* P.eof

  where
    values = do
      vs <- P.sepBy1 inner $ P.choice [
        P.try $ skipSpaces *> (P.char ',') *> skipSpaces
      , skipSomeSpaces
      ]

      pure $ case vs of
            Cons x Nil -> x
            _          -> ArrayValue (toUnfoldable vs)

    inner = do
      P.try value <|> do
        StringValue <$> do
          foldMap String.singleton <$> do
            many $ P.try (P.noneOf [',', ' ', '\n'])

    value = P.choice $ P.try <$> [ bool, number, quoted ]

    number = do
      si <- P.option 1 (P.char '-' *> pure (-1))
      xs <- foldMap String.singleton <$> some digit
      P.choice [
        FloatValue <<< ((Int.toNumber si) * _) <<< readFloat <$> do
          xss <- do
            P.char '.'
            foldMap String.singleton <$> some digit
          pure $ xs <> "." <> xss
      , pure $ IntValue $ si * (unsafePartial $ fromJust $ Int.fromString xs)
      ]


    bool = true' <|> false'
      where
        true' = do
          P.choice $ P.try <<< P.string <$> [ "true", "True", "TRUE" ]
          pure $ BoolValue true
        false' = do
          P.choice $ P.try <<< P.string <$> [ "false", "False", "FALSE" ]
          pure $ BoolValue false

    quoted = StringValue <$> do
      foldMap String.singleton <$> do
        P.choice [
          P.between (P.char '"')  (P.char '"')  (many $ P.noneOf ['"'])
        , P.between (P.char '\'') (P.char '\'') (many $ P.noneOf ['\''])
        ]

-- | Optimal: Faster P.skipSpaces since it does not accumulate into a list.
space = P.satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
skipSpaces = go
  where go = (do space
                 go) <|> pure unit
skipSomeSpaces = space *> skipSpaces

