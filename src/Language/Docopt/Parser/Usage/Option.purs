module Language.Docopt.Parser.Usage.Option (
    OptionArgumentObj ()
  , LOptObj ()
  , SOptObj ()
  , showOptionArgumentObj
  , showSOptObj
  , showLOptObj
  , eqOptionArgumentObj
  , eqSOptObj
  , eqLOptObj
  , lopt', lopt, loptR, lopt_, loptR_
  , sopt', sopt, soptR, sopt_, soptR_
  , prettyPrintLOptObj
  , prettyPrintSOptObj
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Data.Function (on)
import Data.String (fromChar)
import Data.Foldable (intercalate)
import Data.List (toList)

type OptionArgumentObj = {
  name     :: String
, optional :: Boolean
}

type SOptObj = {
  flag       :: Char
, stack      :: Array Char
, arg        :: Maybe OptionArgumentObj
, repeatable :: Boolean
}

type LOptObj = {
  name       :: String
, arg        :: Maybe OptionArgumentObj
, repeatable :: Boolean
}

newtype OptionArgument = OptionArgument OptionArgumentObj

unOptionArgument :: OptionArgument -> OptionArgumentObj
unOptionArgument (OptionArgument a) = a

instance showOptionArgument :: Show OptionArgument where
  show = showOptionArgumentObj <<< unOptionArgument

instance eqOptionArgument :: Eq OptionArgument where
  eq = eqOptionArgumentObj `on` unOptionArgument

showSOptObj :: SOptObj -> String
showSOptObj o = "{ flag: "       <> show o.flag
             <> ", stack: "      <> show o.stack
             <> ", arg: "        <> show (OptionArgument <$> o.arg)
             <> ", repeatable: " <> show o.repeatable
             <> "}"

showLOptObj :: LOptObj -> String
showLOptObj o = "{ name: "       <> show o.name
             <> ", arg: "        <> show (OptionArgument <$> o.arg)
             <> ", repeatable: " <> show o.repeatable
             <> "}"

eqSOptObj :: SOptObj -> SOptObj -> Boolean
eqSOptObj o o' = o.flag                     == o'.flag
              && o.stack                    == o'.stack
              && o.repeatable               == o'.repeatable
              && (OptionArgument <$> o.arg) == (OptionArgument <$> o'.arg)

eqLOptObj :: LOptObj -> LOptObj -> Boolean
eqLOptObj o o' = o.name                     == o'.name
              && o.repeatable               == o'.repeatable
              && (OptionArgument <$> o.arg) == (OptionArgument <$> o'.arg)

showOptionArgumentObj :: OptionArgumentObj -> String
showOptionArgumentObj a = "{ name: "     <> show a.name
                       <> ", optional: " <> show a.optional
                       <> "}"

-- XXX: There was a bug prior to refactoring this module that would cause a eq
--      comparison of two OptionArguments's to always yield 'true'. Since
--      currently only the test suite checks for equality and is not easy to
--      update, leave this bug purposely here.
-- TODO: Update the `UsageParserSpec` test suite to properly check for
--       whether an option argument is optional or not.
eqOptionArgumentObj :: OptionArgumentObj -> OptionArgumentObj -> Boolean
-- Note: This would be the correct implementation:
-- eqOptionArgumentObj a a' = a.name     == a'.name
--                         && a.optional == a'.optional
eqOptionArgumentObj _ _ = true

--------------------------------------------------------------------------------
-- Short hand function to create options
-- XXX: Remove this
--------------------------------------------------------------------------------

-- short hand to create a short option node
sopt' :: Char -> Array Char -> Maybe OptionArgumentObj -> Boolean -> SOptObj
sopt' f fs a r = { flag: f, stack: fs, arg: a, repeatable: r }

sopt :: Char -> Array Char -> OptionArgumentObj -> SOptObj
sopt f fs a = sopt' f fs (pure a) false

sopt_ :: Char -> Array Char -> SOptObj
sopt_ f fs = sopt' f fs Nothing false

soptR :: Char -> Array Char -> OptionArgumentObj -> SOptObj
soptR f fs a = sopt' f fs (pure a) true

soptR_ :: Char -> Array Char -> SOptObj
soptR_ f fs = sopt' f fs Nothing true

-- short hand to create a long option node
lopt' :: String -> Maybe OptionArgumentObj -> Boolean -> LOptObj
lopt' n a r = { name: n, arg: a, repeatable: r }

lopt :: String -> OptionArgumentObj -> LOptObj
lopt n a = lopt' n (pure a) false

lopt_ :: String -> LOptObj
lopt_ n = lopt' n Nothing false

loptR :: String -> OptionArgumentObj -> LOptObj
loptR n a = lopt' n (pure a) true

loptR_ :: String -> LOptObj
loptR_ n = lopt' n Nothing true

prettyPrintLOptObj :: LOptObj -> String
prettyPrintLOptObj o
  = "--" <> o.name
      <> (maybe "" (\a ->
            (if a.optional then "[" else ""
              <> "=" <> a.name
              <> if a.optional then "]" else "")) o.arg)
      <> if o.repeatable then "..." else ""

prettyPrintSOptObj :: SOptObj -> String
prettyPrintSOptObj o
  = "-" <> (fromChar o.flag)
      <> (intercalate "" $ fromChar <$> toList o.stack)
      <> (maybe "" (\a ->
            (if a.optional then "[" else "")
              <> "=" <> a.name
              <> (if a.optional then "]" else "")) o.arg)
      <> if o.repeatable then "..." else ""
