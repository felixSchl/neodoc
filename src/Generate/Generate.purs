module Docopt.Generate (
  mkBranchParser
, runCliParser
) where

import Prelude
import Control.Monad.State (State(), evalState)
import Data.Either (Either(..), either)
import Data.List (List(..), foldM, (:), singleton)
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
    -- Options always transition to the `Pending state`
    step (Free p) x@(Option _ _ _ _ _) = Right $ Pending p (singleton x)

    -- Any other argument causes immediate evaluation
    step (Free p) x = Right $ Free do
      a  <- p
      as <- (mkP x)
      -- XXX: Do sth with `a` and `as` here!
      pure unit

    -- Options always keep accumulating
    step (Pending p xs) x@(Option _ _ _ _ _) = Right $
      Pending p (x:xs)

    -- Any non-options always leaves the pending state
    step (Pending p xs) y = Right $
      Free do
        a  <- p
        -- TODO: Create a parser that continues to consume elements from a list
        --       until the list is exhausted! The parser for each `x` in `xs`
        --       can be retrieved by `mkP <$> xs`, however the tricky part is
        --       parsing the list until it has been totally consumed.
        -- as <- ???
        pure unit

    mkP (Command n) = do
      -- XXX: Match string against input here!
      pure unit
    mkP (Positional n r) = do
      -- XXX: Match against "valid" input here! "Valid" is any string that
      --      that is not an option.
      -- XXX: Consider `r` here - we must either return a list, always, or
      --      a data type that ensures that at least one value is present:
      --      `X Xs`, where both `X` and `Xs` are data constructors.
      pure unit
    mkP (Option f n a d r) = do
      -- XXX: Match against input:
      --    ["--fooBAR", ...] == ["--foo", "BAR"]
      --    ["-fFILE", ...]   == ["-f", "FILE", ...]
      --
      --    1. See if this option's long name exists and if it is a substring
      --        of the input, or
      --    2. see if this option's long name exists and if it is a substring
      --        of the input.
      --    3. If match is found, consume next input string from stream.
      -- XXX: Consider `r` here - we must either return a list, always, or
      --      a data type that ensures that at least one value is present:
      --      `X Xs`, where both `X` and `Xs` are data constructors.
      pure unit
    mkP (Group o bs r) = 
      -- XXX: Recursively generate a parser.
      --      NOTE: THIS COMPUTATION MAY FAIL:
      --            `mkP` must be in the Either monad
      -- XXX: Consider `r` here - we must either return a list, always, or
      --      a data type that ensures that at least one value is present:
      --      `X Xs`, where both `X` and `Xs` are data constructors.
      pure unit
