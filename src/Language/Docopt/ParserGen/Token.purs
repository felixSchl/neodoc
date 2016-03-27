module Language.Docopt.ParserGen.Token (
    Token (..)
  , PositionedToken (..)
  , prettyPrintToken
  , unPositionedToken
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (intercalate)
import qualified Data.Array as A
import Data.List (List(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple())
import qualified Data.Array as A
import qualified Text.Parsing.Parser     as P
import qualified Text.Parsing.Parser.Pos as P

import qualified Language.Docopt.Errors   as D
import qualified Language.Docopt.Value    as D
import qualified Language.Docopt.Argument as D

-- | Represents each item in ARGV
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | EOA (List D.Value)
  | Stdin
  | Lit String

instance showToken :: Show Token where
  show (LOpt s a)    = "LOpt " ++ show s ++ " " ++ show a
  show (SOpt c cs a) = "SOpt " ++ show c ++ " " ++ show cs ++ " " ++ show a
  show (Lit  s)      = "Lit "  ++ show s
  show (EOA  xs)     = "EOA "  ++ show xs
  show (Stdin)       = "Stdin"

prettyPrintToken :: Token -> String
prettyPrintToken (Stdin) = "-"
prettyPrintToken (EOA xs) = "-- " ++ intercalate " " (D.prettyPrintValue <$> xs)
prettyPrintToken (Lit s) = show s
prettyPrintToken (LOpt n a) = "--" ++ n ++ arg
  where arg = maybe "" ("=" ++) a
prettyPrintToken (SOpt n s a) = "-"  ++ (fromCharArray (A.cons n s)) ++ arg
  where arg = maybe "" ("=" ++) a

data PositionedToken = PositionedToken
  { sourcePos :: P.Position
  , token     :: Token
  }

unPositionedToken :: PositionedToken -> { sourcePos :: P.Position
                                        , token     :: Token
                                        }
unPositionedToken (PositionedToken t) = t

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken { sourcePos=pos, token=tok }) =
    (show tok) ++ " at " ++ (show pos)


