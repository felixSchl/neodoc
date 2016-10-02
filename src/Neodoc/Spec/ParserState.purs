module Neodoc.Spec.ParserState where

import Prelude

type Line = Int
type Indentation = Int
data ParserState = ParserState Indentation Line

setIndentation :: ParserState -> Indentation -> ParserState
setIndentation (ParserState _ l) i = ParserState i l

setLine :: ParserState -> Line -> ParserState
setLine (ParserState i _) l = ParserState i l

getLine :: ParserState -> Line
getLine (ParserState _ l) = l

getIndentation :: ParserState -> Indentation
getIndentation (ParserState i _) = i

initialState :: ParserState
initialState = ParserState 0 0
