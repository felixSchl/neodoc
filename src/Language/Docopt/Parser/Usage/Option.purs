module Language.Docopt.Parser.Usage.Option (
    Argument ()
  , LOpt (..)
  , SOpt (..)
  , lopt', lopt, loptR, lopt_, loptR_
  , sopt', sopt, soptR, sopt_, soptR_
  , prettyPrintLOpt
  , prettyPrintSOpt
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromChar)
import Data.Foldable (intercalate)
import Data.List (List(..), toList)
import Data.Generic

type Argument = String

newtype SOpt = SOpt {
  flag       :: Char
, stack      :: Array Char
, arg        :: Maybe Argument
, repeatable :: Boolean
}

newtype LOpt = LOpt {
  name       :: String
, arg        :: Maybe Argument
, repeatable :: Boolean
}

derive instance genericSOpt :: Generic SOpt
derive instance genericLOpt :: Generic LOpt

instance showLOpt :: Show LOpt
  where show = gShow

instance showSOpt :: Show SOpt
  where show = gShow

instance eqLOpt :: Eq LOpt
  where eq = gEq

instance eqSOpt :: Eq SOpt
  where eq = gEq

-- short hand to create a short option node
sopt' :: Char -> Array Char -> Maybe Argument -> Boolean -> SOpt
sopt' f fs a r = SOpt { flag: f, stack: fs, arg: a, repeatable: r }

sopt :: Char -> Array Char -> String -> SOpt
sopt f fs a = sopt' f fs (pure a) false

sopt_ :: Char -> Array Char -> SOpt
sopt_ f fs = sopt' f fs Nothing false

soptR :: Char -> Array Char -> String -> SOpt
soptR f fs a = sopt' f fs (pure a) true

soptR_ :: Char -> Array Char -> SOpt
soptR_ f fs = sopt' f fs Nothing true

-- short hand to create a long option node
lopt' :: String -> Maybe Argument -> Boolean -> LOpt
lopt' n a r = LOpt { name: n, arg: a, repeatable: r }

lopt :: String -> String -> LOpt
lopt n a = lopt' n (pure a) false

lopt_ :: String -> LOpt
lopt_ n = lopt' n Nothing false

loptR :: String -> String -> LOpt
loptR n a = lopt' n (pure a) true

loptR_ :: String -> LOpt
loptR_ n = lopt' n Nothing true

prettyPrintLOpt :: LOpt -> String
prettyPrintLOpt (LOpt o)
  = "--" ++ o.name
      ++ (maybe "" ("=" ++) o.arg)
      ++ if o.repeatable then "..." else ""

prettyPrintSOpt :: SOpt -> String
prettyPrintSOpt (SOpt o)
  = "-" ++ (fromChar o.flag)
      ++ (intercalate "" $ fromChar <$> toList o.stack)
      ++ (maybe "" ("=" ++) o.arg)
      ++ if o.repeatable then "..." else ""
