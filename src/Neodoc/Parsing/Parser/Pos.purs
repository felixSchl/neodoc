module Neodoc.Parsing.Parser.Pos where

import Prelude
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

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> Position
updatePosString pos str = foldl updatePosChar pos (S.split (wrap "") str)
  where
  updatePosChar (Position line col) c = case c of
    "\n" -> Position (line + 1) 1
    "\r" -> Position (line + 1) 1
    "\t" -> Position line       (col + 8 - ((col - 1) `mod` 8))
    _    -> Position line       (col + 1)
