module Language.Docopt.Trans.Rich (
    reduce
  , RichValue
  ) where

import Data.List (List(), catMaybes, singleton)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))
import Data.Map (Map())
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Bifunctor (lmap)
import Data.Foldable (foldl, maximum)
import Control.Alt ((<|>))

import Language.Docopt.Value (Value(..))
import Language.Docopt.Value as Value
import Language.Docopt.Usage (Usage(Usage), runUsage) as D
import Language.Docopt.Option as O
import Language.Docopt.Env (Env)
import Language.Docopt.Env as Env
import Language.Docopt.Argument (Argument(..), Branch(..), isRepeatable,
                                setRepeatable, runBranch, setRepeatableOr,
                                isCommand, isFlag) as D
import Language.Docopt.ParserGen.ValueMapping (ValueMapping)
import Language.Docopt.Trans.Origin as Origin
import Language.Docopt.Trans.Origin (Origin())
import Language.Docopt.Trans.Key (Key(..), key)

newtype RichValue = RichValue {
  value  :: Value
, origin :: Origin
}

import Prelude

reduce :: List D.Usage       -- ^ the program specification
       -> Env                -- ^ the environment
       -> D.Branch           -- ^ the matched specification
       -> List ValueMapping  -- ^ the parse result
       -> StrMap RichValue   -- ^ the output set of (arg => val)
reduce us env b vs =
  let vm = Map.fromFoldableWith mergeVals $ fromArgv vs
      m  = applyValues vm $ reduceUsage (D.Usage (singleton b))
    -- in expandMap m
   in StrMap.empty

  where

  fromArgv :: List ValueMapping -> List (Tuple Key RichValue)
  fromArgv vs = lmap key <$> (go <$> vs)
    where
    go (Tuple a v) = Tuple a $ RichValue {
      origin: Origin.Argv
    , value:  if D.isRepeatable a
                  then ArrayValue (Value.intoArray v)
                  else v
    }

  mergeVals :: RichValue -> RichValue -> RichValue
  mergeVals (RichValue v) (RichValue v') = RichValue $ {
    origin: fromJust $ maximum [ v.origin, v'.origin ]
  , value:  ArrayValue $ Value.intoArray v.value
                      ++ Value.intoArray v'.value
  }

  applyValues :: Map Key RichValue -> List D.Argument -> Map Key RichValue
  applyValues vm as = Map.fromFoldableWith mergeVals
    $ catMaybes
    $ as <#> \a -> Tuple (key a) <$> do
      (RichValue v) <- (getValue vm a) <|> (getFallback a)
      return $ RichValue v {
        value = if D.isRepeatable a
                    then ArrayValue $ Value.intoArray v.value
                    else v.value
      }
    where

    getValue :: Map Key RichValue -> D.Argument -> Maybe RichValue
    getValue vm a = Map.lookup (key a) vm

    getFallback :: D.Argument -> Maybe RichValue
    getFallback a = getEnvValue a -- <|> getDefaultValue a

      where
      getEnvValue :: D.Argument -> Maybe RichValue
      getEnvValue (D.Option (O.Option o@{ env: Just k })) = do
        s <- Env.lookup k env
        return $ RichValue {
          origin: Origin.Environment
        , value:  StringValue s
        }
      getEnvValue _ = Nothing

-- | Reduce a usage application specification down to a list of unique
-- | arguments, merging declarations where requried. This will later indicate
-- | wether or not to collect values in to an array, etc.
-- |
-- | XXX: Explain more here how merging is done and how repetition is merged
-- |      and so on.
reduceUsage :: D.Usage -> List D.Argument
reduceUsage = Map.values <<< reduceBranches <<< D.runUsage

    where
    reduceBranches :: List D.Branch -> Map Key D.Argument
    reduceBranches bs =
      let ms = combine <<< (expand <$> _) <<< D.runBranch <$> bs
      in foldl (Map.unionWith resolveOR)
                Map.empty
                ms
      where
      combine :: List (Map Key D.Argument) -> Map Key D.Argument
      combine xs = foldl (Map.unionWith resolveLCD)
                          Map.empty
                          xs

    expand :: D.Argument -> Map Key D.Argument
    expand (D.Group _ bs r) =
      reduceBranches
        $ D.Branch <<< ((flip D.setRepeatableOr r) <$> _)
                    <<< D.runBranch
                    <$> bs

    expand arg = Map.singleton (key arg) arg

    resolveOR :: D.Argument -> D.Argument -> D.Argument
    resolveOR (D.Option (O.Option o))
              (D.Option (O.Option o'))
        = D.Option (O.Option o {
                    arg = do
                      a  <- O.runArgument <$> (o.arg  <|> o'.arg)
                      a' <- O.runArgument <$> (o'.arg <|> o.arg )
                      return $ O.Argument {
                        name:     a.name
                      , default:  a.default  <|> a'.default
                      , optional: a.optional || a'.optional
                      }
                    , repeatable = o.repeatable || o'.repeatable
                    })
    resolveOR a b = D.setRepeatable a (D.isRepeatable a || D.isRepeatable b)

    resolveLCD :: D.Argument -> D.Argument -> D.Argument
    resolveLCD (D.Option (O.Option o))
              (D.Option (O.Option o'))
        = D.Option (O.Option o {
                    arg = do
                      a  <- O.runArgument <$> (o.arg  <|> o'.arg)
                      a' <- O.runArgument <$> (o'.arg <|> o.arg )
                      return $ O.Argument {
                        name:     a.name
                      , default:  a.default  <|> a'.default
                      , optional: a.optional || a'.optional
                      }
                    , repeatable = true
                    })
    resolveLCD a b =  D.setRepeatable a true
