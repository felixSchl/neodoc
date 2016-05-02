module Language.Docopt.Value (
    Value (..)
  , read
  , parse
  , isSameValueType
  , isBoolValue
  , isArrayValue
  , prettyPrintValue
  ) where

import Prelude
import Data.Generic (class Generic)
import Data.Either (Either(), either)
import Data.Maybe.Unsafe (fromJust)
import Data.List (List(..), fromList, many, some)
import Control.Apply ((*>))
import Control.Alt ((<|>))
import Text.Parsing.Parser (ParseError, runParser) as P
import Text.Parsing.Parser.Combinators (between, choice, try, sepBy1, option) as P
import Text.Parsing.Parser.String (noneOf, char, string, eof) as P
import Language.Docopt.Parser.Base (digit)
import Data.Array as A
import Data.Int (toNumber, fromString) as Int
import Data.String (fromCharArray)
import Global (readFloat)

data Value
  = StringValue String
  | BoolValue   Boolean
  | ArrayValue  (Array Value)
  | IntValue    Int
  | FloatValue  Number

derive instance genericValue :: Generic Value

instance showValue :: Show Value where
  show (StringValue s) = "StringValue " ++ s
  show (BoolValue   b) = "BoolValue "   ++ show b
  show (ArrayValue xs) = "ArrayValue "  ++ show (show <$> xs)
  show (IntValue    x) = "IntValue "    ++ show x
  show (FloatValue  x) = "FloatValue "  ++ show x

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

read :: String  -- ^ the input
     -> Boolean -- ^ allow splitting?
     -> Value
read s split = either (const $ StringValue s) id (parse s split)

-- | Parse a string into a value
-- | Values can be command *AND* space separated:
-- |
-- | a  b  c -> [ a, b, c ]
-- | a, b, c -> [ a, b, c ]
-- | a  b, c -> [ a, b, c ]
-- | a, b  c -> [ a, b, c ]
-- |
parse :: String  -- ^ the input
      -> Boolean -- ^ allow splitting?
      -> Either P.ParseError Value
parse s split = P.runParser s do
                  v <- if split then values
                                else value
                  P.eof
                  return v

  where
    values = do
      vs <- P.sepBy1 inner $ P.choice [
        P.try $ many white *> (P.char ',') *> many white
      , some $ white
      ]

      return $ case vs of
            Cons x Nil -> x
            _          -> ArrayValue (fromList vs)

    white = P.char ' ' <|> P.char '\n'

    inner = do
      P.try value <|> do StringValue <$> do
                                  fromCharArray <<< fromList <$> do
                                    many $ P.try (P.noneOf [',', ' ', '\n'])

    value = P.choice $ P.try <$> [ bool, number, quoted ]

    number = do
      si <- P.option 1 (P.char '-' *> return (-1))
      xs <- fromCharArray <$> A.some digit
      P.choice [
        FloatValue <<< ((Int.toNumber si) * _) <<< readFloat <$> do
          xss <- do
            P.char '.'
            fromCharArray <$> A.some digit
          return $ xs ++ "." ++ xss
      , return $ IntValue $ si * (fromJust $ Int.fromString xs)
      ]


    bool = true' <|> false'
      where
        true' = do
          P.choice $ P.try <<< P.string <$> [ "true", "True", "TRUE" ]
          return $ BoolValue true
        false' = do
          P.choice $ P.try <<< P.string <$> [ "false", "False", "FALSE" ]
          return $ BoolValue false

    quoted = StringValue <$> do
      fromCharArray <<< fromList <$> do
        P.choice [
          P.between (P.char '"')  (P.char '"')  (many $ P.noneOf ['"'])
        , P.between (P.char '\'') (P.char '\'') (many $ P.noneOf ['\''])
        ]
