module Neodoc.ArgParser.Token where

import Prelude
import Data.Generic
import Data.Function (on)
import Data.Pretty (class Pretty, pretty)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(), maybe)
import Data.Foldable (intercalate)
import Data.List (List())
import Data.String (fromCharArray)
import Data.Array as A

import Neodoc.Value (Value)
type IsNegated = Boolean
data Token
  = LOpt String IsNegated (Maybe String)
  | SOpt Char (Array Char) IsNegated (Maybe String)
  | EOA (List Value)
  | Stdin
  | Lit String

derive instance ordToken :: Ord Token
derive instance eqToken :: Eq Token
derive instance genericToken :: Generic Token

instance showToken :: Show Token where
  show = gShow

instance prettyToken :: Pretty Token where
  pretty (Stdin) = "-"
  pretty (EOA xs) = "-- " <> intercalate " " (pretty <$> xs)
  pretty (Lit s) = s
  pretty (LOpt n neg a) = sign <> n <> arg
    where sign = if neg then "--no-" else "--"
          arg = maybe "" ("=" <> _) a
  pretty (SOpt n s neg a) = sign <> (fromCharArray (A.cons n s)) <> arg
    where sign = if neg then "+" else "-"
          arg = maybe "" ("=" <> _) a

data PositionedToken = PositionedToken Token String Int

derive instance eqPositionedToken :: Eq PositionedToken
derive instance ordPositionedToken :: Ord PositionedToken
derive instance genericPositionedToken :: Generic PositionedToken

instance showPositionedToken :: Show PositionedToken where
  show = gShow

instance prettyPositionedToken :: Pretty PositionedToken where
  pretty (PositionedToken tok _ _) = pretty tok

getSource :: PositionedToken -> String
getSource (PositionedToken _ s _) = s

getToken :: PositionedToken -> Token
getToken (PositionedToken t _ _) = t
