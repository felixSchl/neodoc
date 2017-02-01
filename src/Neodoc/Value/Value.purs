module Neodoc.Value (
    Value (..)
  , read
  , parse
  , isSameValueType
  , isBoolValue
  , isArrayValue
  , prettyPrintValue
  , intoArray
  , prettyType
  , fromValue
  , fromValueAs
  , ReadError
  , class FromValue
  ) where

import Prelude
import Global
import Data.Generic
import Data.Optimize.Uncurried
import Data.Int (toNumber, fromNumber)
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromJust)
import Data.List (List(..), toUnfoldable, many, some, (:))
import Data.Foldable (foldMap)
import Data.Traversable (for)
import Control.Apply ((*>), (<*))
import Control.Alt ((<|>))
import Data.Array as A
import Data.Int (toNumber, fromString) as Int
import Data.String (singleton, toUpper, trim) as String
import Partial.Unsafe (unsafePartial)
import Data.Pretty (class Pretty, pretty)
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Data.Function.Memoize

import Neodoc.Parsing.Parser.Pos as P
import Neodoc.Parsing.Parser (Parser(..), ParserArgs(..), Step(..))
import Neodoc.Parsing.Parser.String (StringParser)
import Neodoc.Parsing.Parser.String as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Parsing.Parser as P

data Value
  = StringValue String
  | BoolValue   Boolean
  | ArrayValue  (Array Value)
  | IntValue    Int
  | FloatValue  Number

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value
derive instance genericValue :: Generic Value

instance showValue :: Show Value where
  show = gShow


instance isForeignValue :: IsForeign Value where
  read v = do
        (BoolValue   <$> F.readBoolean v)
    <|> (IntValue    <$> F.readInt     v)
    <|> (FloatValue  <$> F.readNumber  v)
    <|> (StringValue <$> F.readString  v)
    <|> (ArrayValue  <$> (F.readArray v >>= \vs -> for vs F.read))
    <|> (F.fail $ F.JSONError "Invalid value")

instance asForeignValue :: AsForeign Value where
  write (BoolValue    v) = F.toForeign v
  write (IntValue     v) = F.toForeign v
  write (FloatValue   v) = F.toForeign v
  write (StringValue  v) = F.toForeign v
  write (ArrayValue  vs) = F.toForeign $ F.write <$> vs

instance prettyValue :: Pretty Value where
  pretty = prettyPrintValue

type ReadError = String

class FromValue a where
  fromValue   :: Value -> Either ReadError a
  fromValueAs :: Value -> Either ReadError a

instance fromValueString :: FromValue String where
  fromValue v = case v of
    StringValue s -> Right s
    v             -> Left $ "Expected string, but got " <> prettyType v
  fromValueAs v = case v of
    StringValue v  -> Right v
    BoolValue   v  -> Right $ show v
    IntValue    v  -> Right $ show v
    FloatValue  v  -> Right $ show v
    ArrayValue  vs -> Left "cannot coerce array to string"

instance fromValueBoolean :: FromValue Boolean where
  fromValue v = case v of
    BoolValue b -> Right b
    v           -> Left $ "Expected boolean, but got " <> prettyType v
  fromValueAs v = case v of
    StringValue v  -> case String.trim (String.toUpper v) of
      "TRUE"  -> Right true
      "FALSE" -> Right false
      _       -> Left $ "Cannot coerce this string to boolean"
    BoolValue   v   -> Right v
    IntValue    0   -> Right false
    IntValue    _   -> Right true
    FloatValue  0.0 -> Right false
    FloatValue  _   -> Right true
    ArrayValue  xs  -> Right $ Array.length xs > 0

instance fromValueInt :: FromValue Int where
  fromValue v = case v of
    IntValue i -> Right i
    v          -> Left $ "Expected int, but got " <> prettyType v
  fromValueAs v = case v of
    StringValue v  ->
      let n = readInt 10 v
       in if isFinite n then case fromNumber n of
                              Just n' -> Right n'
                              _       -> Left "string is not an integer"
                        else Left "string is not a number"
    IntValue    v  -> Right v
    BoolValue   v  -> Right if v then 1 else 0
    FloatValue  _  -> Left $ "cannot coerce float to int"
    ArrayValue  _  -> Left $ "cannot coerce array to int"

instance fromValueNumber :: FromValue Number where
  fromValue v = case v of
    FloatValue f -> Right f
    IntValue   i -> Right $ toNumber i
    v            -> Left $ "Expected float, but got " <> prettyType v
  fromValueAs v = case v of
    StringValue v  ->
      let n = readFloat v
       in if isFinite n then Right n
                        else Left "string is not a number"
    IntValue    v  -> Right $ toNumber v
    BoolValue   v  -> Right if v then 1.0 else 0.0
    FloatValue  _  -> Left $ "cannot coerce float to float"
    ArrayValue  _  -> Left $ "cannot coerce array to float"

instance fromValueArray :: (FromValue a) => FromValue (Array a) where
  fromValue v = case v of
    ArrayValue xs -> for (Array.mapWithIndex (/\) xs) \(i /\ x) ->
      lmap (\s -> "Unexpected type at index " <> show i <> ": " <> s) $
        fromValue x
    v -> Left $ "Expected array, but got " <> prettyType v
  fromValueAs v = case v of
    ArrayValue xs -> for (Array.mapWithIndex (/\) xs) \(i /\ x) ->
      lmap (\s -> "Unexpected type at index " <> show i <> ": " <> s) $
        fromValueAs x
    v -> Left $ "Expected array, but got " <> prettyType v

isSameValueType :: Value -> Value -> Boolean
isSameValueType (StringValue _) (StringValue _) = true
isSameValueType (BoolValue   _) (BoolValue   _) = true
isSameValueType (ArrayValue  _) (ArrayValue  _) = true
isSameValueType (IntValue    _) (IntValue    _) = true
isSameValueType (FloatValue  _) (FloatValue  _) = true
isSameValueType _               _               = false

prettyType :: Value -> String
prettyType (StringValue _) = "string"
prettyType (BoolValue   _) = "boolean"
prettyType (ArrayValue  _) = "array"
prettyType (IntValue    _) = "int"
prettyType (FloatValue  _) = "float"

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
prettyPrintValue (StringValue s) = show s
prettyPrintValue (BoolValue   b) = show b
prettyPrintValue (IntValue    i) = show i
prettyPrintValue (FloatValue  f) = show f
prettyPrintValue (ArrayValue xs) = "[" <> (A.intercalate ", " $ prettyPrintValue <$> xs) <> "]"

intoArray (ArrayValue xs) = xs
intoArray v               = [v]

read :: String  -- ^ the input
     -> Boolean -- ^ allow splitting?
     -> Value
read s split = either (const $ StringValue s) id (parse s split)

-- | Parser that parses strings
type StringParser' a = StringParser String Unit Unit a

-- | Parse a string into a value
-- | Values can be comma *AND* space separated:
-- |
-- | a  b  c -> [ a, b, c ]
-- | a, b, c -> [ a, b, c ]
-- | a  b, c -> [ a, b, c ]
-- | a, b  c -> [ a, b, c ]
-- |
parse
  :: String  -- ^ the input
  -> Boolean -- ^ allow splitting?
  -> Either String Value
parse s split =
  let p = if split then values else value <* P.eof
  in lmap (P.extractError id) $
      P.runParser' (Args5 unit P.initialPos unit s p)

  where
    values = do
      vs <- P.sepBy1 inner $ P.choice [
        P.try $ skipSpaces *> (P.char ',') *> skipSpaces
      , skipSomeSpaces
      ]

      pure $ case vs of
            x:Nil -> x
            _     -> ArrayValue (toUnfoldable vs)

    inner = do
      P.try value <|> do
        StringValue <$> do
          foldMap String.singleton <$> do
            P.many $ P.try (P.noneOf [',', ' ', '\n'])

    value = P.choice $ P.try <$> [ bool, number, quoted ]

    number = do
      si <- P.option 1 (P.char '-' *> pure (-1))
      xs <- foldMap String.singleton <$> P.some P.digit
      P.choice [
        FloatValue <<< ((Int.toNumber si) * _) <<< readFloat <$> do
          xss <- do
            P.char '.'
            foldMap String.singleton <$> P.some P.digit
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
          P.between (P.char '"')  (P.char '"')  (P.many $ P.noneOf ['"'])
        , P.between (P.char '\'') (P.char '\'') (P.many $ P.noneOf ['\''])
        ]

-- | Optimal: Faster P.skipSpaces since it does not accumulate into a list.
space = P.satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
skipSpaces = go
  where go = (do space
                 go) <|> pure unit
skipSomeSpaces = space *> skipSpaces
