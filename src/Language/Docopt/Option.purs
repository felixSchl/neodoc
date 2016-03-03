module Language.Docopt.Option (
    Argument (..)
  , Option (..)
  , Flag ()
  , Name ()
  , IsOptional ()
  , IsRepeatable ()
  , isRepeatable
  , hasDefault
  , isFlag
  , takesArgument
  , prettyPrintOption
  , opt',  opt,  opt_,  optR,  optR_
  , lopt', lopt, lopt_, loptR, loptR_
  , sopt', sopt, sopt_, soptR, soptR_
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
  name    :: String
, default :: Maybe Value
}

newtype Option = Option {
  flag       :: Maybe Flag
, name       :: Maybe Name
, arg        :: Maybe Argument
, repeatable :: IsRepeatable
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
isFlag _ = false

prettyPrintOption :: Option -> String
prettyPrintOption (Option o)
  = short ++ long ++ arg' ++ rep ++ default
  where
    short   = maybe "" (\f -> "-" ++ (fromChar f)) o.flag
    long    = maybe "" (const ", ") (o.flag *> o.name)
              ++ maybe "" ("--" ++) o.name
    rep     = if o.repeatable then "..." else ""
    arg'    = flip (maybe "") o.arg \(Argument { name }) -> "="  ++ name
    default = flip (maybe "") o.arg \(Argument { default }) ->
                flip (maybe "") default \v->
                  " [default: " ++ (prettyPrintValue v) ++  "]"

-- short hand to create an Option argument
opt' :: Maybe Flag -> Maybe Name -> Maybe Argument -> IsRepeatable -> Option
opt' f n a r = Option { flag: f, name: n, arg: a, repeatable: r }

opt :: Flag -> Name -> Argument -> Option
opt f n a = opt' (Just f) (Just n) (Just a) false

optR :: Flag -> Name -> Argument -> Option
optR f n a = opt' (Just f) (Just n) (Just a) true

opt_ :: Flag -> Name -> Option
opt_ f n = opt' (Just f) (Just n) Nothing false

optR_ :: Flag -> Name -> Option
optR_ f n = opt' (Just f) (Just n) Nothing true

-- short hand to create an Short-Option argument
sopt' :: Flag -> (Maybe Argument) -> IsRepeatable -> Option
sopt' f a r = Option { flag: pure f, name: Nothing, arg: a, repeatable: r }

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
lopt' n a r = Option { flag: Nothing, name: pure n, arg: a, repeatable: r }

lopt :: Name -> Argument -> Option
lopt n a = lopt' n (Just a) false

loptR :: Name -> Argument -> Option
loptR n a = lopt' n (Just a) true

lopt_ :: Name -> Option
lopt_ n = lopt' n Nothing false

loptR_ :: Name -> Option
loptR_ n = lopt' n Nothing true

