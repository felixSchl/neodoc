module Language.Docopt.Argument.Option (
    Flag ()
  , Name ()
  , OptionObj ()
  , OptionArgumentObj ()
  , IsOptional ()
  , IsRepeatable ()
  , showOptionObj
  , eqOptionObj
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

import Prelude
import Data.Function (on)
import Control.Apply ((*>))
import Data.Maybe (Maybe(..), maybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (fromChar)

import Language.Docopt.Value (Value(..), prettyPrintValue)

type Flag         = Char
type Name         = String
type IsRepeatable = Boolean
type IsOptional   = Boolean

type OptionObj =  { flag       :: Maybe Flag
                  , name       :: Maybe Name
                  , arg        :: Maybe OptionArgumentObj
                  , env        :: Maybe String
                  , repeatable :: IsRepeatable
                  }

showOptionObj :: OptionObj -> String
showOptionObj o = "{ flag: "       <> show o.flag
               <> ", name: "       <> show o.name
               <> ", arg: "        <> show (OptionArgument <$> o.arg)
               <> ", env: "        <> show o.env
               <> ", repeatable: " <> show o.repeatable
               <> "}"

eqOptionObj :: OptionObj -> OptionObj -> Boolean
eqOptionObj o o' = o.flag               == o'.flag
                && o.name               == o'.name
                && o.env                == o'.env
                && o.repeatable         == o'.repeatable
                && (OptionArgument <$> o.arg) == (OptionArgument <$> o'.arg)

newtype OptionArgument = OptionArgument OptionArgumentObj

unOptionArgument :: OptionArgument -> OptionArgumentObj
unOptionArgument (OptionArgument a) = a

instance showOptionArgument :: Show OptionArgument where
  show = showOptionArgumentObj <<< unOptionArgument

instance eqOptionArgument   :: Eq   OptionArgument where
  eq   = eqOptionArgumentObj `on` unOptionArgument

type OptionArgumentObj = { name     :: String
                   , default  :: Maybe Value
                   , optional :: Boolean
                   }

showOptionArgumentObj :: OptionArgumentObj -> String
showOptionArgumentObj a = "{ name: "     <> show a.name
                       <> ", default: "  <> show a.default
                       <> ", optional: " <> show a.optional
                       <> "}"

eqOptionArgumentObj :: OptionArgumentObj -> OptionArgumentObj -> Boolean
eqOptionArgumentObj a a' = a.name     == a'.name
                  && a.default  == a'.default
                  && a.optional == a'.optional

empty :: OptionObj
empty = { flag:       Nothing
        , name:       Nothing
        , arg:        Nothing
        , env:        Nothing
        , repeatable: false
        }

hasDefault :: OptionObj -> Boolean
hasDefault { arg: Just { default: Just _ } } = true
hasDefault _                                 = false

takesArgument :: OptionObj -> Boolean
takesArgument { arg: Just _ } = true
takesArgument _               = false

isFlag :: OptionObj -> Boolean
isFlag { arg: Just { default: Just (BoolValue _)}} = true
isFlag { arg: Nothing }                            = true
isFlag _                                           = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption o
  = short ++ long ++ arg' ++ rep ++ default ++ env
  where
    short = maybe "" (\f -> "-" ++ (fromChar f)) o.flag
    long  = maybe "" (const ", ") (o.flag *> o.name)
              ++ maybe "" ("--" ++ _) o.name
    rep  = if o.repeatable then "..." else ""
    arg' = flip (maybe "") o.arg \({ name, optional }) ->
                (if optional then "[" else "")
                  ++ "=" ++ name
                  ++ (if optional then "]" else "")
    default = flip (maybe "") o.arg \({ default }) ->
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
    arg' = flip (maybe "") o.arg \({ name }) -> "="  ++ name

--------------------------------------------------------------------------------
-- Short hand option creation
--
-- XXX: Remove this (!)
--------------------------------------------------------------------------------

-- short hand to create an Option argument
opt'
  :: Maybe Flag
  -> Maybe Name
  -> Maybe OptionArgumentObj
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

opt :: Flag -> Name -> OptionArgumentObj -> OptionObj
opt f n a = opt' (Just f) (Just n) (Just a) Nothing false

optR :: Flag -> Name -> OptionArgumentObj -> OptionObj
optR f n a = opt' (Just f) (Just n) (Just a) Nothing true

opt_ :: Flag -> Name -> OptionObj
opt_ f n = opt' (Just f) (Just n) Nothing Nothing false

optR_ :: Flag -> Name -> OptionObj
optR_ f n = opt' (Just f) (Just n) Nothing Nothing true

-- short hand to create an Short-OptionObj argument
sopt' :: Flag -> (Maybe OptionArgumentObj) -> IsRepeatable -> OptionObj
sopt' f a r = {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        Nothing
, repeatable: r
}

sopt :: Flag -> OptionArgumentObj -> OptionObj
sopt f a = sopt' f (Just a) false

soptR :: Flag -> OptionArgumentObj -> OptionObj
soptR f a = sopt' f (Just a) true

sopt_ :: Flag -> OptionObj
sopt_ f = sopt' f Nothing false

soptR_ :: Flag -> OptionObj
soptR_ f = sopt' f Nothing true

-- short hand to create an Long-OptionObj argument
lopt' :: Name -> (Maybe OptionArgumentObj) -> IsRepeatable -> OptionObj
lopt' n a r = {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        Nothing
, repeatable: r
}

lopt :: Name -> OptionArgumentObj -> OptionObj
lopt n a = lopt' n (Just a) false

loptR :: Name -> OptionArgumentObj -> OptionObj
loptR n a = lopt' n (Just a) true

lopt_ :: Name -> OptionObj
lopt_ n = lopt' n Nothing false

loptR_ :: Name -> OptionObj
loptR_ n = lopt' n Nothing true

--------------------------------------------------------------------------------
-- Short hand option creation (with env tag)
--------------------------------------------------------------------------------

optE :: Flag -> Name -> OptionArgumentObj -> String -> OptionObj
optE f n a e = opt' (Just f) (Just n) (Just a) (Just e) false

optER :: Flag -> Name -> OptionArgumentObj -> String -> OptionObj
optER f n a e = opt' (Just f) (Just n) (Just a) (Just e) true

optE_ :: Flag -> Name -> String -> OptionObj
optE_ f n e = opt' (Just f) (Just n) Nothing (Just e) false

optER_ :: Flag -> Name -> String -> OptionObj
optER_ f n e = opt' (Just f) (Just n) Nothing (Just e) true

-- short hand to create an Short-OptionObj argument
soptE' :: Flag -> (Maybe OptionArgumentObj) -> IsRepeatable -> String -> OptionObj
soptE' f a r e = {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        pure e
, repeatable: r
}

soptE :: Flag -> OptionArgumentObj -> String -> OptionObj
soptE f a e = soptE' f (Just a) false e

soptER :: Flag -> OptionArgumentObj -> String -> OptionObj
soptER f a e = soptE' f (Just a) true e

soptE_ :: Flag -> String -> OptionObj
soptE_ f e = soptE' f Nothing false e

soptER_ :: Flag -> String -> OptionObj
soptER_ f e = soptE' f Nothing true e

-- short hand to create an Long-OptionObj argument
loptE' :: Name -> (Maybe OptionArgumentObj) -> IsRepeatable -> String -> OptionObj
loptE' n a r e = {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        pure e
, repeatable: r
}

loptE :: Name -> OptionArgumentObj -> String -> OptionObj
loptE n a e = loptE' n (Just a) false e

loptER :: Name -> OptionArgumentObj -> String -> OptionObj
loptER n a e = loptE' n (Just a) true e

loptE_ :: Name -> String -> OptionObj
loptE_ n e = loptE' n Nothing false e

loptER_ :: Name -> String -> OptionObj
loptER_ n e = loptE' n Nothing true e
