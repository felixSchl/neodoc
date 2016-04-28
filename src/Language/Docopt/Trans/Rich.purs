module Language.Docopt.Trans.Rich (
    reduce
  , RichValue
  ) where

import Prelude

import Data.List (List(), catMaybes, singleton, concat, toList)
import Data.Array (length, filter) as A
import Data.Maybe (Maybe(..), fromMaybe)
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
import Language.Docopt.Trans.Key (Key(..), key, toKeys)

newtype RichValue = RichValue {
  value  :: Value
, origin :: Origin
}

reduce :: List D.Usage       -- ^ the program specification
       -> Env                -- ^ the environment
       -> D.Branch           -- ^ the matched specification
       -> List ValueMapping  -- ^ the parse result
       -> StrMap RichValue   -- ^ the output set of (arg => val)
reduce us env b vs =
  let vm = Map.fromFoldableWith mergeVals $ fromArgv vs
      m  = applyValues vm $ reduceUsage (D.Usage (singleton b))
    in finalFold m

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
      (RichValue v) <-  getValue vm     a
                    <|> getEnvValue     a
                    <|> getDefaultValue a
                    <|> getEmptyValue   a

      return $ RichValue v {
        value = if D.isRepeatable a
                    then ArrayValue $ Value.intoArray v.value
                    else v.value
      }
    where

    getValue :: Map Key RichValue -> D.Argument -> Maybe RichValue
    getValue vm a = Map.lookup (key a) vm

    getEnvValue :: D.Argument -> Maybe RichValue
    getEnvValue (D.Option (O.Option o@{ env: Just k })) = do
      s <- Env.lookup k env
      return $ RichValue {
        origin: Origin.Environment
      , value:  StringValue s
      }
    getEnvValue _ = Nothing

    getDefaultValue :: D.Argument -> Maybe RichValue
    getDefaultValue (D.Option (O.Option o@{
        arg: Just (O.Argument { default: Just v })
      })) = return
              $ RichValue {
                  origin: Origin.Default
                , value: if (o.repeatable)
                            then ArrayValue $ Value.intoArray v
                            else v
                }
    getDefaultValue _ = Nothing

    getEmptyValue :: D.Argument -> Maybe RichValue
    getEmptyValue x = RichValue <<< { origin: Origin.Empty
                                    , value:  _
                                    } <$> go x
      where
      go (D.Option (O.Option o@{ arg: Nothing }))
        = return
            $ if o.repeatable then ArrayValue []
                              else BoolValue false
      go (D.Positional _ r) | r = return $ ArrayValue []
      go (D.Command _ r)    | r = return $ ArrayValue []
      go (D.Stdin)              = return $ BoolValue false
      go (D.EOA)                = return $ ArrayValue []
      go _                      = Nothing

  finalFold :: Map Key RichValue -> StrMap RichValue
  finalFold m =
    StrMap.fromFoldable $ concat
      $ Map.toList m
          <#> \(Tuple (Key { arg: a }) (RichValue rv)) ->
                  let v = fromMaybe rv.value do
                        if D.isFlag a || D.isCommand a
                          then case rv.value of
                            ArrayValue xs ->
                              return
                                $ IntValue (A.length $ flip A.filter xs \x ->
                                    case x of
                                        BoolValue b -> b
                                        _           -> false
                                  )
                            BoolValue b ->
                              if D.isRepeatable a
                                  then
                                    return $ if b
                                              then IntValue 1
                                              else IntValue 0
                                  else Nothing
                            _ -> Nothing
                          else Nothing
                    in flip Tuple (RichValue $ rv { value = v }) <$> do
                        toList $ toKeys a


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
      in foldl (Map.unionWith resolveAcrossBranches)
                Map.empty
                ms
      where
      combine :: List (Map Key D.Argument) -> Map Key D.Argument
      combine xs = foldl (Map.unionWith resolveInSameBranch)
                          Map.empty
                          xs

    expand :: D.Argument -> Map Key D.Argument
    expand (D.Group _ bs r) =
      reduceBranches
        $ D.Branch <<< ((flip D.setRepeatableOr r) <$> _)
                    <<< D.runBranch
                    <$> bs

    expand arg = Map.singleton (key arg) arg

    resolveAcrossBranches :: D.Argument -> D.Argument -> D.Argument
    resolveAcrossBranches (D.Option (O.Option o))
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
    resolveAcrossBranches a b = D.setRepeatable a (D.isRepeatable a || D.isRepeatable b)

    resolveInSameBranch :: D.Argument -> D.Argument -> D.Argument
    resolveInSameBranch (D.Option (O.Option o))
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
    resolveInSameBranch a b =  D.setRepeatable a true
