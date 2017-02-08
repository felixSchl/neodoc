module Neodoc.ArgParser.Fallback (
  getFallbackValue
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List)
import Control.Alt ((<|>))
import Neodoc.Env as Env
import Neodoc.Env (Env ())
import Neodoc.Value as Value
import Neodoc.Value (Value(..))
import Neodoc.Value.Origin as Origin
import Neodoc.Value.Origin (Origin())
import Neodoc.Value.RichValue (RichValue())
import Neodoc.Value.RichValue (from, getOrigin) as RValue
import Neodoc.Data.SolvedLayout
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.OptionArgument
import Neodoc.Data.Description
import Neodoc.Data.Description as Description
import Neodoc.ArgParser.Options (Options)

-- Find a fallback value for the given argument.
getFallbackValue
  :: ∀ r
   . Options r
  -> Env
  -> Maybe Description
  -> SolvedLayoutArg
  -> Maybe RichValue
getFallbackValue options env mDescription x = do
  (fromEnv       mDescription <#> RValue.from Origin.Environment) <|>
  (fromDefault x mDescription <#> RValue.from Origin.Default)     <|>
  (empty       x              <#> RValue.from Origin.Empty)

  where
  fromEnv :: Maybe Description -> Maybe Value
  fromEnv (Just (OptionDescription _ _ _ _ (Just k))) = StringValue <$> Env.lookup k env
  fromEnv _                                           = Nothing

  fromDefault :: SolvedLayoutArg -> Maybe Description -> Maybe Value
  fromDefault (Option _ _ r) (Just (OptionDescription _ _ _ (Just v) _))
    = pure if r
              then ArrayValue $ Value.intoArray v
              else v
  fromDefault _ _ = Nothing

  empty :: SolvedLayoutArg -> Maybe Value
  empty = go
    where
    go (Option _ Nothing r)
      | not options.requireFlags
      = pure if r then ArrayValue []
                  else BoolValue false
    go (Option _ (Just (OptionArgument _ true)) r)
      | not options.requireFlags
      = pure if r then ArrayValue []
                  else BoolValue false
    go Stdin   = pure $ BoolValue false
    go _       = Nothing
