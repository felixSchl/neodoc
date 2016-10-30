module Neodoc.ArgParser.Debug where

import Prelude
import Debug.Trace hiding (trace)
import Data.String as String
import Data.Pretty
import Data.List (List)
import Data.List.Lazy as LL
import Neodoc.ArgParser.Type
import Neodoc.ArgParser.Token

_ENABLE_DEBUG_ :: Boolean
_ENABLE_DEBUG_ = false

trace :: ∀ r. Int -> (List PositionedToken -> String) -> ArgParser r Unit
trace l f = if _ENABLE_DEBUG_
              then do
                input       <- getInput
                state       <- getState
                globalState <- getGlobalState
                traceA $ indent l <> stateLabel state globalState <> (f input)
              else pure unit

traceError :: ∀ r a. Int -> String -> ArgParser r a  -> ArgParser r a
traceError l s = catch' \st e ->
  trace l (\_ -> "! " <> s <> ": " <> pretty e)
    *> setState st
    *> throw e

traceInput :: ∀ r. ArgParser r Unit
traceInput = traceA =<< pretty <$> getInput

indent :: Int -> String
indent l = String.fromCharArray $ LL.toUnfoldable $ LL.take (l) $ LL.repeat ' '

traceBracket
  :: ∀ r a
   . (Pretty a)
  => Int
  -> String
  -> ArgParser r a
  -> ArgParser r a
traceBracket l label p = do
  input       <- getInput
  state       <- getState
  globalState <- getGlobalState
  trace l \_ ->
    stateLabel state globalState <> " parsing " <> label <> " (input: " <> pretty input <> ")"
  output <- traceError l (stateLabel state globalState <> " failed to parse " <> label) p
  input'       <- getInput
  state'       <- getState
  globalState' <- getGlobalState
  trace l \_ ->
    stateLabel state' globalState' <> " successfully parsed " <> label <> "!"
      <> " (output: " <> pretty output <> ")"
      <> " (new input: " <> pretty input' <> ")"
  pure output

stateLabel :: ArgParseState -> GlobalArgParseState -> String
stateLabel { hasTerminated, depth } { deepestError } =
  (if hasTerminated then "✓" else "·")
  -- <> "(" <> show depth <> ")"
  -- <> "(dE = " <> show (pretty <$> deepestError) <> ")
