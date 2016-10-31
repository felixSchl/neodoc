module Neodoc.Spec.ParserState where

import Prelude

type Line = Int
type Indentation = Int
data ParserState = ParserState Indentation Line

setIndentation :: Indentation -> ParserState -> ParserState
setIndentation i (ParserState _ l) = ParserState i l

setLine :: Line -> ParserState -> ParserState
setLine l (ParserState i _) = ParserState i l

getLine :: ParserState -> Line
getLine (ParserState _ l) = l

getIndentation :: ParserState -> Indentation
getIndentation (ParserState i _) = i

initialState :: ParserState
initialState = ParserState 0 0
