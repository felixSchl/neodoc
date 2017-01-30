module Neodoc.ArgParser.Token where

import Prelude
import Data.Generic
import Data.Maybe
import Data.Function (on)
import Data.Pretty (class Pretty, pretty)
import Data.Tuple.Nested ((/\), uncurry2, uncurry3)
import Data.Maybe (Maybe(), maybe)
import Data.Foldable (intercalate)
import Data.List (List(Nil))
import Data.String (fromCharArray)
import Data.Array as A
import Data.Function.Memoize
import Data.Lazy

import Neodoc.Value (Value)
data Token
  = LOpt String (Maybe String)
  | SOpt Char (Array Char) (Maybe String)
  | EOA (List Value)
  | Stdin
  | Lit String

derive instance ordToken :: Ord Token
derive instance eqToken :: Eq Token
derive instance genericToken :: Generic Token

{-
  Tabulate a Token. We only tabulate the identifying part of the token.
-}
instance tabulateToken :: Tabulate Token where
  tabulate f = let lopt = tabulate (f <<< flip LOpt Nothing)
                   fSOpt (a /\ b) = SOpt a b Nothing
                   sopt = tabulate (f <<< fSOpt)
                   lit  = tabulate (f <<< Lit)
                in case _ of
                      LOpt s _    -> lopt s
                      SOpt c cs _ -> sopt (c /\ cs)
                      EOA _       -> defer (\_ -> f $ EOA Nil)
                      Stdin       -> defer (\_ -> f $ Stdin)
                      Lit s       -> lit s

instance showToken :: Show Token where
  show = gShow

instance prettyToken :: Pretty Token where
  pretty (Stdin) = "-"
  pretty (EOA xs) = "-- " <> intercalate " " (pretty <$> xs)
  pretty (Lit s) = s
  pretty (LOpt n a) = "--" <> n <> arg
    where arg = maybe "" ("=" <> _) a
  pretty (SOpt n s a) = "-"  <> (fromCharArray (A.cons n s)) <> arg
    where arg = maybe "" ("=" <> _) a

data PositionedToken = PositionedToken Token String Int

derive instance eqPositionedToken :: Eq PositionedToken
derive instance ordPositionedToken :: Ord PositionedToken
derive instance genericPositionedToken :: Generic PositionedToken

instance showPositionedToken :: Show PositionedToken where
  show = gShow

instance prettyPositionedToken :: Pretty PositionedToken where
  pretty (PositionedToken tok _ _) = pretty tok

{-
  Tabulate a Positioned Token. We only tabulate the identifying part of the token.
-}
instance tabulatePositionedToken :: Tabulate PositionedToken where
  tabulate f = let c (a /\ b /\ c) = PositionedToken a b c
                   g = tabulate (f <<< c)
                in case _ of
                    PositionedToken t s i -> g (t /\ s /\ i)

getSource :: PositionedToken -> String
getSource (PositionedToken _ s _) = s

getToken :: PositionedToken -> Token
getToken (PositionedToken t _ _) = t
