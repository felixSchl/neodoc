module Docopt.Parser.State where

import Prelude

data ParserState = ParserState {
  indentation :: Int -- The active indentation level
}

instance showParserState :: Show ParserState where
  show (ParserState { indentation: indent }) =
    "{ indentation: " ++ show indent ++  " }"
