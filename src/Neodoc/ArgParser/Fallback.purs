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

-- import Language.Docopt.Argument (Argument(..), OptionArgument(..)) as D
import Neodoc.Data.Description
import Neodoc.ArgParser.Options (Options)

-- Find a fallback value for the given argument.
getFallbackValue
  :: ∀ r
   . Options r
  -> Env
  -> List Description
  -> SolvedLayoutArg
  -> Maybe RichValue
getFallbackValue _ _ _ _ = Nothing

-- getFallbackValue options env x = do
--   (fromEnv     x <#> RValue.from Origin.Environment) <|>
--   (fromDefault x <#> RValue.from Origin.Default)     <|>
--   (empty       x <#> RValue.from Origin.Empty)
--
--   where
--   fromEnv :: D.Argument -> Maybe Value
--   fromEnv (D.Option (o@{ env: Just k })) = StringValue <$> Env.lookup k env
--   fromEnv _                              = Nothing
--
--   fromDefault :: D.Argument -> Maybe Value
--   fromDefault (D.Option (o@{ arg: Just (D.OptionArgument { default: Just v }) }))
--     = pure if o.repeatable
--               then ArrayValue $ Value.intoArray v
--               else v
--   fromDefault _ = Nothing
--
--   empty :: D.Argument -> Maybe Value
--   empty = go
--     where
--     go (D.Option (o@{ arg: Nothing }))
--       | not options.requireFlags
--       = pure if o.repeatable  then ArrayValue []
--                               else BoolValue false
--     go (D.Option (o@{ arg: Just (D.OptionArgument { optional: true }) }))
--       | not options.requireFlags
--       = pure if o.repeatable  then ArrayValue []
--                               else BoolValue false
--     go (D.Stdin) = pure $ BoolValue false
--     go (D.EOA)   = pure $ ArrayValue []
--     go _         = Nothing
