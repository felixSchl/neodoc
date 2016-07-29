module Language.Docopt.ArgParser.Parser.Token (
  token
  ) where

import Prelude
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (PState(..), ParseError(..), ParserT(..), Result(..),
                            parseFailed) as P
import Language.Docopt.ArgParser.Token (getSource) as Token
import Language.Docopt.ArgParser.Token (PositionedToken(..), Token(..))
import Language.Docopt.ArgParser.Parser.Types

-- | Test the token at the head of the stream
token :: ∀ a. (Token -> Maybe a) -> Parser a
token test = P.ParserT $ \(P.PState toks ppos) ->
  pure $ case toks of
    (PositionedToken { token: tok }):xs ->
      case test tok of
        Just a ->
          let nextpos =
                case xs of
                  (PositionedToken { sourcePos: npos }):_ -> npos
                  _                                       -> ppos
          in P.Result xs (Right a) true nextpos
        -- XXX: Fix this error message, it makes no sense!
        Nothing -> P.parseFailed toks ppos "a better error message!"
    _ -> P.parseFailed toks ppos "Expected token, met EOF"

