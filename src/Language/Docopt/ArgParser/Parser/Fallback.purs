module Language.Docopt.ArgParser.Parser.Fallback (
  getFallbackValue
  ) where

import Prelude
import Data.Maybe
import Control.Alt ((<|>))
import Language.Docopt.Env as Env
import Language.Docopt.Env (Env ())
import Language.Docopt.Value as Value
import Language.Docopt.Value (Value(..))
import Language.Docopt.Origin as Origin
import Language.Docopt.Origin (Origin())
import Language.Docopt.RichValue (RichValue())
import Language.Docopt.RichValue (from, getOrigin) as RValue
import Language.Docopt.Argument (Argument(..), OptionArgument(..)) as D
import Language.Docopt.ArgParser.Parser.Options (Options())

-- Find a fallback value for the given argument.
getFallbackValue :: ∀ r. Options r -> Env -> D.Argument -> Maybe RichValue
getFallbackValue options env x = do
  (fromEnv     x <#> RValue.from Origin.Environment) <|>
  (fromDefault x <#> RValue.from Origin.Default)     <|>
  (empty       x <#> RValue.from Origin.Empty)

  where
  fromEnv :: D.Argument -> Maybe Value
  fromEnv (D.Option (o@{ env: Just k })) = StringValue <$> Env.lookup k env
  fromEnv _                              = Nothing

  fromDefault :: D.Argument -> Maybe Value
  fromDefault (D.Option (o@{ arg: Just (D.OptionArgument { default: Just v }) }))
    = pure if o.repeatable
              then ArrayValue $ Value.intoArray v
              else v
  fromDefault _ = Nothing

  empty :: D.Argument -> Maybe Value
  empty = go
    where
    go (D.Option (o@{ arg: Nothing }))
      | not options.requireFlags
      = pure if o.repeatable  then ArrayValue []
                              else BoolValue false
    go (D.Option (o@{ arg: Just (D.OptionArgument { optional: true }) }))
      | not options.requireFlags
      = pure if o.repeatable  then ArrayValue []
                              else BoolValue false
    go (D.Stdin) = pure $ BoolValue false
    go (D.EOA)   = pure $ ArrayValue []
    go _         = Nothing
