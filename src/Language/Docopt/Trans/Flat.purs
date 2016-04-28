module Language.Docopt.Trans.Flat (
  reduce
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (foldl)
import Data.Bifunctor (lmap)
import Data.Array as A
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.String (fromChar)
import Data.Map as Map
import Data.Map (Map())
import Data.String as Str
import Data.Tuple (Tuple(Tuple))
import Control.Alt ((<|>))
import Language.Docopt.Usage (Usage(Usage), runUsage) as D
import Language.Docopt.Env (Env) as D
import Language.Docopt.Value (Value(..), isArrayValue) as D
import Language.Docopt.Argument (Argument(..), Branch(..), isRepeatable,
                                setRepeatable, runBranch, setRepeatableOr,
                                isCommand, isFlag) as D
import Language.Docopt.Option as O
import Language.Docopt.Env as Env
import Data.List (List, singleton, catMaybes, toList, concat)
import Language.Docopt.ParserGen.ValueMapping (ValueMapping)
import Language.Docopt.Trans.Key (Key(..), key, toKeys)

-- | Reduce the list of (Argument, Value) mappings down to a Set of (String,
-- | Value), where `String` is the name of the argument.
-- |
-- | This reduction is "lossy" as it loses some of the rich information
-- | about the options that have been acquired. It also loses the information
-- | about the origin of the matched value as it could have been user-provded or
-- | derived from the option descriptions (default value, etc.).
-- |
-- | Despite the pitfalls, the reasons for adopting this are:
-- |
-- |   a) Compatibility with original docopt
-- |   b) Easy of use in JS-land. This step could implemented differently for
-- |      consumption in purescript (or any other language for that matter).
-- |
-- | A note on equality:
-- |    Options are considered equal if their names (or both of their aliases)
-- |    match. Modifiers such as repetitiveness or the arguments an option takes
-- |    are *NOT* considered for evaluating option equality.
-- |
reduce :: List D.Usage       -- ^ the program specification
       -> D.Env              -- ^ the environment
       -> D.Branch           -- ^ the matched specification
       -> List ValueMapping  -- ^ the parse result
       -> StrMap D.Value     -- ^ the output set of (arg => val)
reduce us env b vs =
  let vm = Map.fromFoldableWith resolveArg $ lmap key <$> (prepare <$> vs)
    -- XXX: The following creation of a list of usages is looking funny because
    --      originally this implementation would collect values across all
    --      usage patterns (`us`). However, the new implementation only
    --      considers the branch the user actually matched. Potentially, both
    --      approaches could be combined? The problem with going cross-usage is
    --      that arguments present in multiple usage patterns would falsy be
    --      identified as repeating.
      m  = applyValues vm $ reduceUsages (singleton $ D.Usage (singleton b))
    in expandMap m
  where

    prepare :: ValueMapping -> ValueMapping
    prepare (Tuple a v) | D.isRepeatable a = Tuple a (D.ArrayValue $ valIntoArray v)
    prepare k = k

    expandMap :: Map Key D.Value -> StrMap D.Value
    expandMap m =
      StrMap.fromFoldable $ concat
        $ Map.toList m
            <#> \(Tuple (Key { arg: a }) v) ->
                    let v' =
                        if D.isFlag a || D.isCommand a
                          then case v of
                            D.ArrayValue xs ->
                              D.IntValue (A.length $ flip A.filter xs \x ->
                                case x of
                                      D.BoolValue b -> b
                                      _ -> false
                              )
                            D.BoolValue b ->
                              if D.isRepeatable a
                                 then if b then D.IntValue 1 else D.IntValue 0
                                 else v
                            _ -> v
                          else v
                     in flip Tuple v' <$> (toList $ toKeys a)

    applyValues :: Map Key D.Value -> List D.Argument -> Map Key D.Value
    applyValues vm as = Map.fromFoldableWith resolveArg
      $ catMaybes
      $ as <#> \a -> Tuple (key a) <$> do
        v <- (getValue vm a <|> getFallback a)
        return $ if D.isRepeatable a
                    then D.ArrayValue $ valIntoArray v
                    else v

    resolveArg :: D.Value -> D.Value -> D.Value
    resolveArg v v' = D.ArrayValue $ valIntoArray v' ++ valIntoArray v

    valIntoArray (D.ArrayValue xs)  = xs
    valIntoArray v                  = [v]

    reduceUsages :: List D.Usage -> List D.Argument
    reduceUsages us =
      let ms = reduceBranches <<< D.runUsage <$> us
       in Map.values $ foldl (Map.unionWith resolveLCD)
                             Map.empty
                             ms
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

    getValue :: Map Key D.Value -> D.Argument -> Maybe D.Value
    getValue vm a = do
      Map.lookup (key a) vm

    getFallback :: D.Argument -> Maybe D.Value
    getFallback a = getEnvValue a <|> getDefaultValue a

    getEnvValue :: D.Argument -> Maybe D.Value
    getEnvValue (D.Option (O.Option o@{ env: Just k }))
      = D.StringValue <$> Env.lookup k env
    getEnvValue _ = Nothing

    getDefaultValue :: D.Argument -> Maybe D.Value
    getDefaultValue (D.Option (O.Option o@{
        arg: Just (O.Argument { default: Just v })
      }))
      = return $
          if (D.isArrayValue v || not o.repeatable) then v
                                                    else D.ArrayValue [v]
    -- Handle option flags. Flags indicate only truthiness via their
    -- presence. Absent flags are always falsy!
    getDefaultValue (D.Option (O.Option o@{ arg: Nothing }))
      = return $ if o.repeatable then D.ArrayValue []
                                 else D.BoolValue false
    getDefaultValue (D.Positional _ r)
      = if r then return $ D.ArrayValue []
             else Nothing
    getDefaultValue (D.Command _ r)
      = if r then return $ D.ArrayValue [ D.BoolValue false ]
             else return $ D.BoolValue false
    getDefaultValue (D.Stdin) = return $ D.BoolValue false
    getDefaultValue (D.EOA) = Just $ D.ArrayValue []
    getDefaultValue _ = Nothing
