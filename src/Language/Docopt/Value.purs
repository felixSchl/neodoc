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
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace
import Data.Generic
import Data.Either (Either(), either)
import Data.Int as Int
import Data.List (List(..), fromList, many, some)
import Data.Maybe (fromMaybe)
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String      as P
import Data.Array as A
import Data.String (fromCharArray)
import Data.String as Str
import Global (isNaN, readFloat)

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

read :: String -> Value
read s = either (const $ StringValue s) id (parse s)

-- | Parse a string into a value
-- | Values can be command *AND* space separated:
-- |
-- | a  b  c -> [ a, b, c ]
-- | a, b, c -> [ a, b, c ]
-- | a  b, c -> [ a, b, c ]
-- | a, b  c -> [ a, b, c ]
-- |
parse :: String -> Either P.ParseError Value
parse = flip P.runParser values

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

    inner =do
      P.try value <|> do StringValue <$> do
                                  fromCharArray <<< fromList <$> do
                                    many $ P.try (P.noneOf [',', ' ', '\n'])

    value = P.choice $ P.try <$> [ bool, number, quoted ]

    number = do
      s <- fromCharArray <$> A.some (P.noneOf [',', ' ', '\n'])
      let n = readFloat s
      if isNaN n then P.fail "NaN"
                 else if (Str.contains "." s)
                        then return (FloatValue n)
                        else return (IntValue   (unsafeCoerce n))


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
