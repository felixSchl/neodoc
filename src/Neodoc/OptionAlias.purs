module Neodoc.OptionAlias (
    OptionAlias (..)
  , Aliases ()
  , isLong
  , isShort
  , toAliasList
  , fromString
  , IsNegative
  , isNegative
  , setNegative
  , module NonEmpty
  ) where

import Prelude
import Data.String as String
import Data.Bifunctor (lmap, bimap)
import Data.Optimize.Uncurried
import Data.Maybe (Maybe(..))
import Data.Array as A
import Data.List (List(Nil), (:))
import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (singleton, fromCharArray) as String
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Pretty (class Pretty)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F

import Neodoc.Parsing.Parser.Pos as P
import Neodoc.Parsing.Parser (Parser(..), ParserArgs(..), Step(..))
import Neodoc.Parsing.Parser.String (StringParser)
import Neodoc.Parsing.Parser.String as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Parsing.Parser as P

type Aliases = NonEmpty List OptionAlias
type IsNegative = Boolean
data OptionAlias = Short Char IsNegative | Long String IsNegative

derive instance eqOptionAlias :: Eq OptionAlias
derive instance ordOptionAlias :: Ord OptionAlias
derive instance genericOptionAlias :: Generic OptionAlias

instance showOptionAlias :: Show OptionAlias where
  show = gShow

instance isForeignOptionAlias :: IsForeign OptionAlias where
  read v = do
    s :: String <- read v
    case fromString s of
      Left  msg -> F.fail $ F.JSONError msg
      Right s   -> pure s

fromString :: String -> Either String OptionAlias
fromString s = lmap (P.extractError id) do
  P.runParser $ Args5 unit P.initialPos unit s $ (_ <$ P.eof) =<< do
    P.choice [
      P.try $ P.char '+' *> do
        c <- P.noneOf [ '-', '+' ]
        P.return $ Short c true
    , P.try $ P.string "--" *> do
        neg <- P.choice [ P.try (P.string "no-") $> true, pure false ]
        n <- String.fromCharArray <$> A.many P.anyChar
        P.return $ Long n neg
    , P.try $ P.char '-' *> do
        c <- P.noneOf [ '-', '+' ]
        P.return $ Short c false
    ]

instance asForeignOptionAlias :: AsForeign OptionAlias where
  write (Short c neg) = F.toForeign $ sign <> (String.singleton c)
    where sign = if neg then "+" else "-"
  write (Long  n neg) = F.toForeign $ sign <> n
    where sign = if neg then "--no-" else "--"

instance prettyOptionAlias :: Pretty OptionAlias where
  pretty (Short c neg) = sign <> (String.singleton c)
    where sign = if neg then "+" else "-"
  pretty (Long  n neg) = sign <> n
    where sign = if neg then "--no-" else "--"

isNegative :: OptionAlias -> Boolean
isNegative (Long _ neg) = neg
isNegative (Short _ neg) = neg

setNegative :: Boolean -> OptionAlias -> OptionAlias
setNegative neg (Long n _) = Long n neg
setNegative neg (Short c _) = Short c neg

isLong :: OptionAlias -> Boolean
isLong (Long _ _) = true
isLong _          = false

isShort :: OptionAlias -> Boolean
isShort (Short _ _) = true
isShort _           = false

toAliasList :: NonEmpty List OptionAlias -> List OptionAlias
toAliasList = fromNonEmpty (:)
