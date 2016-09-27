module Neodoc.ArgParser.Token (
    Token (..)
  , PositionedToken (..)
  , getSource
  , unPositionedToken
  ) where

import Prelude
import Data.Function (on)
import Data.Pretty (class Pretty, pretty)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(), maybe)
import Data.Foldable (intercalate)
import Data.List (List())
import Data.String (fromCharArray)
import Data.Array as A
import Text.Parsing.Parser.Pos (Position(..)) as P

import Neodoc.Value (Value)
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | EOA (List Value)
  | Stdin
  | Lit String

instance showToken :: Show Token where
  show (LOpt s a)    = "LOpt " <> show s <> " " <> show a
  show (SOpt c cs a) = "SOpt " <> show c <> " " <> show cs <> " " <> show a
  show (Lit  s)      = "Lit "  <> show s
  show (EOA  xs)     = "EOA "  <> show xs
  show (Stdin)       = "Stdin"

instance ordToken :: Ord Token where
  compare = compare `on` show -- XXX

instance eqToken :: Eq Token where
  eq = eq `on` show -- XXX

instance prettyToken :: Pretty Token where
  pretty (Stdin) = "-"
  pretty (EOA xs) = "-- " <> intercalate " " (pretty <$> xs)
  pretty (Lit s) = show s
  pretty (LOpt n a) = "--" <> n <> arg
    where arg = maybe "" ("=" <> _) a
  pretty (SOpt n s a) = "-"  <> (fromCharArray (A.cons n s)) <> arg
    where arg = maybe "" ("=" <> _) a

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  , source    :: String
  }

instance ordPositionedToken :: Ord PositionedToken where
  compare = compare `on` \(PositionedToken {
    sourcePos: P.Position l r
  , token
  , source
  }) -> l /\ r /\ source /\ token

instance eqPositionedToken :: Eq PositionedToken where
  eq = eq `on` \(PositionedToken {
    sourcePos
  , token
  , source
  }) -> source /\ token /\ sourcePos

unPositionedToken
  :: PositionedToken
  ->  { sourcePos :: P.Position
      , token     :: Token
      , source    :: String
      }
unPositionedToken (PositionedToken t) = t

getSource :: PositionedToken -> String
getSource = _.source <<< unPositionedToken

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken { sourcePos, token, source }) =
    "PositionedToken { "
      <> "sourcePos: " <> show sourcePos
      <> ", token: " <> show token
      <> ", source: " <> show source
      <> " }"

instance prettyPositionedToken :: Pretty PositionedToken where
  pretty (PositionedToken { token }) = pretty token
