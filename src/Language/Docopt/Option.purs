module Language.Docopt.Option (
    Argument (..)
  , Flag ()
  , Name ()
  , IsOptional ()
  , OptionObj ()
  , IsRepeatable ()
  , runArgument
  , isRepeatable
  , hasDefault
  , isFlag
  , takesArgument
  , prettyPrintOption
  , prettyPrintOptionNaked
  , empty
  , opt',   opt,   opt_,   optR,   optR_
  , lopt',  lopt,  lopt_,  loptR,  loptR_
  , sopt',  sopt,  sopt_,  soptR,  soptR_
  ,         optE,  optE_,  optER,  optER_
  , loptE', loptE, loptE_, loptER, loptER_
  , soptE', soptE, soptE_, soptER, soptER_
  ) where

import Prelude (class Eq, class Show, pure, (++), flip, const)
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromChar)
import Control.Apply ((*>))
import Data.Generic (class Generic, gEq, gShow)

import Language.Docopt.Value (Value(..), prettyPrintValue)

type Flag = Char
type Name = String
type IsRepeatable = Boolean
type IsOptional = Boolean

newtype Argument = Argument {
  name     :: String
, default  :: Maybe Value
, optional :: Boolean
}

runArgument :: Argument -> { name     :: String
                           , default  :: Maybe Value
                           , optional :: Boolean
                           }
runArgument (Argument a) = a

type OptionObj =  { flag       :: Maybe Flag
                  , name       :: Maybe Name
                  , arg        :: Maybe Argument
                  , env        :: Maybe String
                  , repeatable :: IsRepeatable
                  }

empty :: OptionObj
empty = { flag:       Nothing
        , name:       Nothing
        , arg:        Nothing
        , env:        Nothing
        , repeatable: false
        }

derive instance genericArgument :: Generic Argument

instance showArgument :: Show Argument where
  show = gShow

instance eqArgument :: Eq Argument where
  eq = gEq

-- TODO: Remove
isRepeatable :: OptionObj -> Boolean
isRepeatable o = o.repeatable

hasDefault :: OptionObj -> Boolean
hasDefault { arg: Just (Argument { default: Just _ }) } = true
hasDefault _                                            = false

takesArgument :: OptionObj -> Boolean
takesArgument { arg: Just _ } = true
takesArgument _               = false

isFlag :: OptionObj -> Boolean
isFlag { arg: Just (Argument { default: Just (BoolValue _)})} = true
isFlag { arg: Nothing }                                       = true
isFlag _                                                      = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption o
  = short ++ long ++ arg' ++ rep ++ default ++ env
  where
    short = maybe "" (\f -> "-" ++ (fromChar f)) o.flag
    long  = maybe "" (const ", ") (o.flag *> o.name)
              ++ maybe "" ("--" ++ _) o.name
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \(Argument { name, optional }) ->
                (if optional then "[" else "")
                  ++ "=" ++ name
                  ++ (if optional then "]" else "")
    default = flip (maybe "") o.arg \(Argument { default }) ->
                flip (maybe "") default \v->
                  " [default: " ++ (prettyPrintValue v) ++  "]"
    env = flip (maybe "") o.env \k -> " [env: " ++ k ++ "]"

prettyPrintOptionNaked :: OptionObj -> String
prettyPrintOptionNaked o
  = short ++ long ++ arg' ++ rep
  where
    short = maybe "" (\f -> "-" ++ (fromChar f)) o.flag
    long  = maybe "" (const "|") (o.flag *> o.name)
              ++ maybe "" ("--" ++ _) o.name
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \(Argument { name }) -> "="  ++ name

--------------------------------------------------------------------------------
-- Short hand option creation
--------------------------------------------------------------------------------

-- short hand to create an Option argument
opt'
  :: Maybe Flag
  -> Maybe Name
  -> Maybe Argument
  -> Maybe String
  -> IsRepeatable
  -> OptionObj
opt' f n a e r = {
  flag:       f
, name:       n
, arg:        a
, env:        e
, repeatable: r
}

opt :: Flag -> Name -> Argument -> OptionObj
opt f n a = opt' (Just f) (Just n) (Just a) Nothing false

optR :: Flag -> Name -> Argument -> OptionObj
optR f n a = opt' (Just f) (Just n) (Just a) Nothing true

opt_ :: Flag -> Name -> OptionObj
opt_ f n = opt' (Just f) (Just n) Nothing Nothing false

optR_ :: Flag -> Name -> OptionObj
optR_ f n = opt' (Just f) (Just n) Nothing Nothing true

-- short hand to create an Short-OptionObj argument
sopt' :: Flag -> (Maybe Argument) -> IsRepeatable -> OptionObj
sopt' f a r = {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        Nothing
, repeatable: r
}

sopt :: Flag -> Argument -> OptionObj
sopt f a = sopt' f (Just a) false

soptR :: Flag -> Argument -> OptionObj
soptR f a = sopt' f (Just a) true

sopt_ :: Flag -> OptionObj
sopt_ f = sopt' f Nothing false

soptR_ :: Flag -> OptionObj
soptR_ f = sopt' f Nothing true

-- short hand to create an Long-OptionObj argument
lopt' :: Name -> (Maybe Argument) -> IsRepeatable -> OptionObj
lopt' n a r = {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        Nothing
, repeatable: r
}

lopt :: Name -> Argument -> OptionObj
lopt n a = lopt' n (Just a) false

loptR :: Name -> Argument -> OptionObj
loptR n a = lopt' n (Just a) true

lopt_ :: Name -> OptionObj
lopt_ n = lopt' n Nothing false

loptR_ :: Name -> OptionObj
loptR_ n = lopt' n Nothing true

--------------------------------------------------------------------------------
-- Short hand option creation (with env tag)
--------------------------------------------------------------------------------

optE :: Flag -> Name -> Argument -> String -> OptionObj
optE f n a e = opt' (Just f) (Just n) (Just a) (Just e) false

optER :: Flag -> Name -> Argument -> String -> OptionObj
optER f n a e = opt' (Just f) (Just n) (Just a) (Just e) true

optE_ :: Flag -> Name -> String -> OptionObj
optE_ f n e = opt' (Just f) (Just n) Nothing (Just e) false

optER_ :: Flag -> Name -> String -> OptionObj
optER_ f n e = opt' (Just f) (Just n) Nothing (Just e) true

-- short hand to create an Short-OptionObj argument
soptE' :: Flag -> (Maybe Argument) -> IsRepeatable -> String -> OptionObj
soptE' f a r e = {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        pure e
, repeatable: r
}

soptE :: Flag -> Argument -> String -> OptionObj
soptE f a e = soptE' f (Just a) false e

soptER :: Flag -> Argument -> String -> OptionObj
soptER f a e = soptE' f (Just a) true e

soptE_ :: Flag -> String -> OptionObj
soptE_ f e = soptE' f Nothing false e

soptER_ :: Flag -> String -> OptionObj
soptER_ f e = soptE' f Nothing true e

-- short hand to create an Long-OptionObj argument
loptE' :: Name -> (Maybe Argument) -> IsRepeatable -> String -> OptionObj
loptE' n a r e = {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        pure e
, repeatable: r
}

loptE :: Name -> Argument -> String -> OptionObj
loptE n a e = loptE' n (Just a) false e

loptER :: Name -> Argument -> String -> OptionObj
loptER n a e = loptE' n (Just a) true e

loptE_ :: Name -> String -> OptionObj
loptE_ n e = loptE' n Nothing false e

loptER_ :: Name -> String -> OptionObj
loptER_ n e = loptE' n Nothing true e
