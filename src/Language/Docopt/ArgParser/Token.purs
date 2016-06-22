module Language.Docopt.ArgParser.Token (
    Token (..)
  , PositionedToken (..)
  , getSource
  , prettyPrintToken
  , unPositionedToken
  ) where

import Prelude
import Data.Maybe (Maybe(), maybe)
import Data.Foldable (intercalate)
import Data.List (List())
import Data.String (fromCharArray)
import Data.Array as A
import Text.Parsing.Parser.Pos (Position) as P

import Language.Docopt.Value (Value, prettyPrintValue) as D
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | EOA (List D.Value)
  | Stdin
  | Lit String

instance showToken :: Show Token where
  show (LOpt s a)    = "LOpt " <> show s <> " " <> show a
  show (SOpt c cs a) = "SOpt " <> show c <> " " <> show cs <> " " <> show a
  show (Lit  s)      = "Lit "  <> show s
  show (EOA  xs)     = "EOA "  <> show xs
  show (Stdin)       = "Stdin"

prettyPrintToken :: Token -> String
prettyPrintToken (Stdin) = "-"
prettyPrintToken (EOA xs) = "-- " <> intercalate " " (D.prettyPrintValue <$> xs)
prettyPrintToken (Lit s) = show s
prettyPrintToken (LOpt n a) = "--" <> n <> arg
  where arg = maybe "" ("=" <> _) a
prettyPrintToken (SOpt n s a) = "-"  <> (fromCharArray (A.cons n s)) <> arg
  where arg = maybe "" ("=" <> _) a

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  , source    :: String
  }

unPositionedToken :: PositionedToken -> { sourcePos :: P.Position
                                        , token     :: Token
                                        , source    :: String
                                        }
unPositionedToken (PositionedToken t) = t

getSource :: PositionedToken -> String
getSource = unPositionedToken >>> _.source

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken { sourcePos: pos, token: tok }) =
    (show tok) <> " at " <> (show pos)


