module Docopt.Parser.State where

import Prelude

type ParserState = {
  indentation :: Int -- The active indentation level
, line        :: Int -- The active line
}
--
-- instance showParserState :: Show ParserState where
--   show (ParserState { indentation: indent, line: line }) =
--     "(" ++ (show line) ++ "|" ++ (show indent) ++ ")"
