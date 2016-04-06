module Language.Docopt.Trans (
  reduce
  ) where

import Prelude
import Debug.Trace
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Foldable (foldl)
import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Array as A
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Map (Map())
import Data.String (fromChar)
import Data.Monoid (mempty)
import Data.Map as Map
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd)
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Data.Function (on)
import Language.Docopt.Env (Env())
import Language.Docopt.Usage    as D
import Language.Docopt.Errors   as D
import Language.Docopt.Env      as D
import Language.Docopt.Value    as D
import Language.Docopt.Argument as D
import Language.Docopt.Option   as O
import Language.Docopt.Env      as Env
import Data.String.Ext ((^=))
import Data.List (List(..), toList, concat, singleton, groupBy, catMaybes)
import Data.List as L
import Language.Docopt.ParserGen.ValueMapping

newtype Key = Key { arg :: D.Argument }

instance showKey :: Show Key where
  show (Key { arg: (D.Option (O.Option o)) }) =
    maybe "" (\c -> fromChar c ++ ", ") o.flag
      ++ maybe "" id o.name
  show (Key { arg: (D.Positional n _) }) = n
  show (Key { arg: (D.Command n _) }) = n
  show _ = "invalid" -- XXX

instance ordKey :: Ord Key where
  compare = compare `on` show -- XXX

instance eqKey :: Eq Key where
  eq (Key { arg: arg0 }) (Key { arg: arg1 }) = go arg0 arg1
    where
      go (D.Positional n _) (D.Positional n' _)
         = n ^= n'
      go (D.Option (O.Option { flag=f,  name=n  }))
         (D.Option (O.Option { flag=f', name=n' }))
         = (f == f') && (n == n')
      go (D.Command n _) (D.Command n' _)
         = n == n'
      go a b = a == b

key :: D.Argument -> Key
key arg = Key { arg: arg }

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
      m  = reduceUsages (singleton $ D.Usage (singleton b)) # applyValues vm
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
                                      D.BoolValue b | b -> true
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
      $ as <#> \a -> Tuple (key a) <$> (getValue vm a <|> getFallback a)

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
          let ms = combine <<< (expand <$>) <<< D.runBranch <$> bs
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
            $ D.Branch <<< ((flip D.setRepeatableOr r) <$>)
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
      v <- Map.lookup (key a) vm
      return $ if D.isRepeatable a
                    then D.ArrayValue $ valIntoArray v
                    else v

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
      = return $ if o.repeatable then D.ArrayValue [ D.BoolValue false ]
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

-- | Derive a key from an argument.
-- | This key is what the user will use to check the value of
-- | a mathed argument.
toKeys :: D.Argument -> Array String
toKeys (D.Command n _)    = [n]
toKeys (D.Positional n _) = [ "<" ++ Str.toLower n ++ ">"
                            , Str.toUpper n
                            ]
toKeys (D.Group _ _ _)    = []
toKeys (D.EOA)            = ["--"]
toKeys (D.Stdin)          = ["-"]
toKeys (D.Option (O.Option o))
                          = []
                          ++ maybe [] (\c -> [ "-"  ++ fromChar c ]) o.flag
                          ++ maybe [] (\s -> [ "--" ++ s ]) o.name

