module Language.Docopt.Parser.Usage.Option (
    OptionArgumentObj ()
  , LOpt (..)
  , SOpt (..)
  , lopt', lopt, loptR, lopt_, loptR_
  , sopt', sopt, soptR, sopt_, soptR_
  , prettyPrintLOpt
  , prettyPrintSOpt
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Data.String (fromChar)
import Data.Foldable (intercalate)
import Data.List (toList)

type OptionArgumentObj = { name     :: String
                         , optional :: Boolean }

newtype SOpt = SOpt {
  flag       :: Char
, stack      :: Array Char
, arg        :: Maybe OptionArgumentObj
, repeatable :: Boolean
}

newtype LOpt = LOpt {
  name       :: String
, arg        :: Maybe OptionArgumentObj
, repeatable :: Boolean
}

instance showSOpt :: Show SOpt where
  show (SOpt o)
    = "SOpt { flag: " <> show o.flag
        <> ", stack: " <> show o.stack
        <> ", repeatable: " <> show o.repeatable
        <> ", argument: " <> (fromMaybe "Nothing" do
                                a <- o.arg
                                return $ "{ name: " <> a.name
                                      <> ", optional: " <> show a.optional
                                      <> "}"
                              )
        <> "}"

instance eqSOpt :: Eq SOpt
  where eq (SOpt o) (SOpt o') = (o.flag       == o'.flag)
                             && (o.stack      == o'.stack)
                             && (o.repeatable == o'.repeatable)
                             && ((isNothing o.arg && isNothing o'.arg)
                                  || (fromMaybe false (do
                                        a  <- o.arg
                                        a' <- o'.arg
                                        return $ (a.name == a.name)
                                              && (a.optional == a.optional)
                                      ))
                                )

instance showLOpt :: Show LOpt where
  show (LOpt o)
    = "LOpt { name: " <> o.name
        <> ", repeatable: " <> show o.repeatable
        <> ", argument: " <> (fromMaybe "Nothing" do
                                a <- o.arg
                                return $ "{ name: " <> a.name
                                      <> ", optional: " <> show a.optional
                                      <> "}"
                              )
        <> "}"


instance eqLOpt :: Eq LOpt
  where eq (LOpt o) (LOpt o') = (o.name       == o'.name)
                             && (o.repeatable == o'.repeatable)
                             && ((isNothing o.arg && isNothing o'.arg)
                                  || (fromMaybe false (do
                                        a  <- o.arg
                                        a' <- o'.arg
                                        return $ (a.name == a.name)
                                              && (a.optional == a.optional)
                                      ))
                                )

--------------------------------------------------------------------------------
-- Short hand function to create options
-- XXX: Remove this
--------------------------------------------------------------------------------

-- short hand to create a short option node
sopt' :: Char -> Array Char -> Maybe OptionArgumentObj -> Boolean -> SOpt
sopt' f fs a r = SOpt { flag: f, stack: fs, arg: a, repeatable: r }

sopt :: Char -> Array Char -> OptionArgumentObj -> SOpt
sopt f fs a = sopt' f fs (pure a) false

sopt_ :: Char -> Array Char -> SOpt
sopt_ f fs = sopt' f fs Nothing false

soptR :: Char -> Array Char -> OptionArgumentObj -> SOpt
soptR f fs a = sopt' f fs (pure a) true

soptR_ :: Char -> Array Char -> SOpt
soptR_ f fs = sopt' f fs Nothing true

-- short hand to create a long option node
lopt' :: String -> Maybe OptionArgumentObj -> Boolean -> LOpt
lopt' n a r = LOpt { name: n, arg: a, repeatable: r }

lopt :: String -> OptionArgumentObj -> LOpt
lopt n a = lopt' n (pure a) false

lopt_ :: String -> LOpt
lopt_ n = lopt' n Nothing false

loptR :: String -> OptionArgumentObj -> LOpt
loptR n a = lopt' n (pure a) true

loptR_ :: String -> LOpt
loptR_ n = lopt' n Nothing true

prettyPrintLOpt :: LOpt -> String
prettyPrintLOpt (LOpt o)
  = "--" <> o.name
      <> (maybe "" (\a ->
            (if a.optional then "[" else ""
              <> "=" <> a.name
              <> if a.optional then "]" else "")) o.arg)
      <> if o.repeatable then "..." else ""

prettyPrintSOpt :: SOpt -> String
prettyPrintSOpt (SOpt o)
  = "-" <> (fromChar o.flag)
      <> (intercalate "" $ fromChar <$> toList o.stack)
      <> (maybe "" (\a ->
            (if a.optional then "[" else "")
              <> "=" <> a.name
              <> (if a.optional then "]" else "")) o.arg)
      <> if o.repeatable then "..." else ""
