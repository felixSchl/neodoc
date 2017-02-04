module Neodoc.Parsing.Parser.Pos where

import Prelude
import Debug.Profile
import Data.Generic
import Data.Foldable (foldl)
import Data.String as S
import Data.Newtype (wrap)

type Line = Int
type Column = Int
data Position = Position Line Column

derive instance genericPosition :: Generic Position
derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position
instance showPosition :: Show Position where
  show = gShow

-- | The `Position` before any input has been parsed.
initialPos :: Position
initialPos = Position 1 1

split :: String -> Array String
split = S.split (wrap "")

updatePosString :: Position -> String -> Position
updatePosString pos str = foldl _updatePosChar pos (split str)

_updatePosChar :: Position -> String -> Position
_updatePosChar (Position line col) c = case c of
  "\n" -> Position (line + 1) 1
  "\r" -> Position (line + 1) 1
  "\t" -> Position line       (col + 8 - ((col - 1) `mod` 8))
  _    -> Position line       (col + 1)
