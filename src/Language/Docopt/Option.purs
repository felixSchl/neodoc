module Language.Docopt.Option (
    Argument (..)
  , Option (..)
  , Flag ()
  , Name ()
  , IsOptional ()
  , IsRepeatable ()
  , runOption
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

import Prelude
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.String (fromChar)
import Control.Apply ((*>))
import qualified Data.String as Str
import Data.Generic

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


newtype Option = Option {
  flag       :: Maybe Flag
, name       :: Maybe Name
, arg        :: Maybe Argument
, env        :: Maybe String
, repeatable :: IsRepeatable
}

runOption :: Option -> { flag       :: Maybe Flag
                       , name       :: Maybe Name
                       , arg        :: Maybe Argument
                       , env        :: Maybe String
                       , repeatable :: IsRepeatable
                       }
runOption (Option o) = o

empty :: { flag       :: Maybe Flag
         , name       :: Maybe Name
         , arg        :: Maybe Argument
         , env        :: Maybe String
         , repeatable :: IsRepeatable
         }
empty = { flag: Nothing
        , name: Nothing
        , arg:  Nothing
        , env:  Nothing
        , repeatable: false
        }

derive instance genericOption :: Generic Option
derive instance genericArgument :: Generic Argument

instance showArgument :: Show Argument where
  show = gShow

instance eqArgument :: Eq Argument where
  eq = gEq

instance showOption :: Show Option where
  show = gShow

instance eqOption :: Eq Option where
  eq = gEq

isRepeatable :: Option -> Boolean
isRepeatable (Option o) = o.repeatable

hasDefault :: Option -> Boolean
hasDefault (Option { arg: Just (Argument a) }) = isJust a.default
hasDefault _                                   = false

takesArgument :: Option -> Boolean
takesArgument (Option { arg: a }) = isJust a
takesArgument _                   = false

isFlag :: Option -> Boolean
isFlag (Option { arg: Just (Argument { default: Just (BoolValue _)})}) = true
isFlag (Option { arg: Just (Argument { optional: true })}) = true
isFlag (Option { arg: Nothing }) = true
isFlag _ = false

prettyPrintOption :: Option -> String
prettyPrintOption (Option o)
  = short ++ long ++ arg' ++ rep ++ default ++ env
  where
    short   = maybe "" (\f -> "-" ++ (fromChar f)) o.flag
    long    = maybe "" (const ", ") (o.flag *> o.name)
              ++ maybe "" ("--" ++) o.name
    rep     = if o.repeatable then "..." else ""
    arg'    = flip (maybe "") o.arg \(Argument { name, optional }) ->
                if optional then "[" else ""
                  ++ "=" ++ name
                  ++ if optional then "]" else ""
    default = flip (maybe "") o.arg \(Argument { default }) ->
                flip (maybe "") default \v->
                  " [default: " ++ (prettyPrintValue v) ++  "]"
    env = flip (maybe "") o.env \k -> " [env: " ++ k ++ "]"

prettyPrintOptionNaked :: Option -> String
prettyPrintOptionNaked (Option o)
  = short ++ long ++ arg' ++ rep
  where
    short   = maybe "" (\f -> "-" ++ (fromChar f)) o.flag
    long    = maybe "" (const "|") (o.flag *> o.name)
              ++ maybe "" ("--" ++) o.name
    rep     = if o.repeatable then "..." else ""
    arg'    = flip (maybe "") o.arg \(Argument { name }) -> "="  ++ name

--------------------------------------------------------------------------------
-- Short hand option creation
--------------------------------------------------------------------------------

-- short hand to create an Option argument
opt' :: Maybe Flag
      -> Maybe Name
      -> Maybe Argument
      -> Maybe String
      -> IsRepeatable
      -> Option
opt' f n a e r = Option {
  flag:       f
, name:       n
, arg:        a
, env:        e
, repeatable: r
}

opt :: Flag -> Name -> Argument -> Option
opt f n a = opt' (Just f) (Just n) (Just a) Nothing false

optR :: Flag -> Name -> Argument -> Option
optR f n a = opt' (Just f) (Just n) (Just a) Nothing true

opt_ :: Flag -> Name -> Option
opt_ f n = opt' (Just f) (Just n) Nothing Nothing false

optR_ :: Flag -> Name -> Option
optR_ f n = opt' (Just f) (Just n) Nothing Nothing true

-- short hand to create an Short-Option argument
sopt' :: Flag -> (Maybe Argument) -> IsRepeatable -> Option
sopt' f a r = Option {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        Nothing
, repeatable: r
}

sopt :: Flag -> Argument -> Option
sopt f a = sopt' f (Just a) false

soptR :: Flag -> Argument -> Option
soptR f a = sopt' f (Just a) true

sopt_ :: Flag -> Option
sopt_ f = sopt' f Nothing false

soptR_ :: Flag -> Option
soptR_ f = sopt' f Nothing true

-- short hand to create an Long-Option argument
lopt' :: Name -> (Maybe Argument) -> IsRepeatable -> Option
lopt' n a r = Option {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        Nothing
, repeatable: r
}

lopt :: Name -> Argument -> Option
lopt n a = lopt' n (Just a) false

loptR :: Name -> Argument -> Option
loptR n a = lopt' n (Just a) true

lopt_ :: Name -> Option
lopt_ n = lopt' n Nothing false

loptR_ :: Name -> Option
loptR_ n = lopt' n Nothing true

--------------------------------------------------------------------------------
-- Short hand option creation (with env tag)
--------------------------------------------------------------------------------

optE :: Flag -> Name -> Argument -> String -> Option
optE f n a e = opt' (Just f) (Just n) (Just a) (Just e) false

optER :: Flag -> Name -> Argument -> String -> Option
optER f n a e = opt' (Just f) (Just n) (Just a) (Just e) true

optE_ :: Flag -> Name -> String -> Option
optE_ f n e = opt' (Just f) (Just n) Nothing (Just e) false

optER_ :: Flag -> Name -> String -> Option
optER_ f n e = opt' (Just f) (Just n) Nothing (Just e) true

-- short hand to create an Short-Option argument
soptE' :: Flag -> (Maybe Argument) -> IsRepeatable -> String -> Option
soptE' f a r e = Option {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        pure e
, repeatable: r
}

soptE :: Flag -> Argument -> String -> Option
soptE f a e = soptE' f (Just a) false e

soptER :: Flag -> Argument -> String -> Option
soptER f a e = soptE' f (Just a) true e

soptE_ :: Flag -> String -> Option
soptE_ f e = soptE' f Nothing false e

soptER_ :: Flag -> String -> Option
soptER_ f e = soptE' f Nothing true e

-- short hand to create an Long-Option argument
loptE' :: Name -> (Maybe Argument) -> IsRepeatable -> String -> Option
loptE' n a r e = Option {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        pure e
, repeatable: r
}

loptE :: Name -> Argument -> String -> Option
loptE n a e = loptE' n (Just a) false e

loptER :: Name -> Argument -> String -> Option
loptER n a e = loptE' n (Just a) true e

loptE_ :: Name -> String -> Option
loptE_ n e = loptE' n Nothing false e

loptER_ :: Name -> String -> Option
loptER_ n e = loptE' n Nothing true e
