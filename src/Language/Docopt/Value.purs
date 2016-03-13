module Language.Docopt.Value (
    Value (..)
  , isSameValueType
  , isBoolValue
  , isArrayValue
  , prettyPrintValue
  ) where

import Prelude
import Data.Generic

data Value
  = StringValue String
  | BoolValue   Boolean
  | ArrayValue  (Array Value)
  | NumberValue Number

derive instance genericValue :: Generic Value

instance showValue :: Show Value where
  show (StringValue s) = "StringValue " ++ s
  show (BoolValue b)   = "BoolValue "   ++ show b
  show (ArrayValue xs) = "ArrayValue "  ++ show (show <$> xs)
  show (NumberValue x) = "NumberValue " ++ show x

instance eqValue :: Eq Value where
  eq (StringValue s) (StringValue s') = (s == s')
  eq (BoolValue b)   (BoolValue b')   = (b == b')
  eq (ArrayValue xs) (ArrayValue xs') = (xs == xs')
  eq (NumberValue x) (NumberValue x') = (x == x')
  eq _               _                = false

isSameValueType :: Value -> Value -> Boolean
isSameValueType (StringValue _) (StringValue _) = true
isSameValueType (BoolValue _)   (BoolValue _)   = true
isSameValueType (ArrayValue _)  (ArrayValue _)  = true
isSameValueType (NumberValue _) (NumberValue _) = true
isSameValueType _               _               = false

isBoolValue :: Value -> Boolean
isBoolValue (BoolValue _) = true
isBoolValue _             = false

isArrayValue :: Value -> Boolean
isArrayValue (ArrayValue _) = true
isArrayValue _              = false

prettyPrintValue :: Value -> String
prettyPrintValue (StringValue s) = s
prettyPrintValue (BoolValue b)   = show b
prettyPrintValue (ArrayValue xs) = show $ prettyPrintValue <$> xs
