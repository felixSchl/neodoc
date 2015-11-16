module Docopt.Generate (
  mkBranchParser
, runCliParser
) where

import Prelude
import Control.Monad.State (State(), evalState)
import Data.Either (Either(..), either)
import Data.List (List(..), foldM, (:))
import qualified Data.List as L

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P

import Docopt

type CliParseState = {}
type CliParser a = P.ParserT (List String) (State CliParseState) a

runCliParser :: forall a.
                (List String)
              -> CliParser a
              -> Either P.ParseError a
runCliParser s =
  flip evalState
  ({})
  <<< P.runParserT
  (P.PState { input: s, position: P.initialPos })

data Acc a
  = Free (CliParser a)
  | Pending (CliParser a) (List Argument)

-- Notes and thoughts:
--
-- CMD OPT POS OPT OPT OPT
--             ^---------^
--                  `- interchangeable
--
-- When needle is on
--      CMD => Generate parser
--      POS => Generate parser
--      GRP => Generate parser recursively
--      OPT => Start accumlating
--
mkBranchParser :: Branch -> CliParser Unit
mkBranchParser (Branch xs) = do
  either
    (\_ -> pure unit)
    (\_ -> pure unit)
    (foldM step (Free $ pure unit) xs)
  where
    -- TODO: Implement parser generation for all terminal arguments
    step (Free p) n = Right $ Free p

    step (Pending p xs) x@(Option _ _ _ _ _) = Right $
      Pending p (x:xs)

    step (Pending p xs) y = Right $
      Free do
        a  <- p
        -- TODO: Create a parser that continues to consume elements from a list
        --       until the list is exhausted.
        -- as <- ???
        pure unit
