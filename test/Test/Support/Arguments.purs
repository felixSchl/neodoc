module Test.Support.Arguments where

import Prelude
import Data.Maybe
import Data.List (List(..), fromFoldable, length, singleton)
import Language.Docopt.Value
import Language.Docopt.Argument

type IsRepeatable = Boolean

-- short hand to create an Option argument
opt'
  :: Maybe Char
  -> Maybe String
  -> Maybe OptionArgumentObj
  -> Maybe String
  -> IsRepeatable
  -> Argument
opt' f n a e r = Option {
  flag:       f
, name:       n
, arg:        a
, env:        e
, repeatable: r
}

opt :: Char -> String -> OptionArgumentObj -> Argument
opt f n a = opt' (Just f) (Just n) (Just a) Nothing false

optR :: Char -> String -> OptionArgumentObj -> Argument
optR f n a = opt' (Just f) (Just n) (Just a) Nothing true

opt_ :: Char -> String -> Argument
opt_ f n = opt' (Just f) (Just n) Nothing Nothing false

optR_ :: Char -> String -> Argument
optR_ f n = opt' (Just f) (Just n) Nothing Nothing true

-- short hand to create an Short-Argument argument
sopt' :: Char -> (Maybe OptionArgumentObj) -> IsRepeatable -> Argument
sopt' f a r = Option {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        Nothing
, repeatable: r
}

sopt :: Char -> OptionArgumentObj -> Argument
sopt f a = sopt' f (Just a) false

soptR :: Char -> OptionArgumentObj -> Argument
soptR f a = sopt' f (Just a) true

sopt_ :: Char -> Argument
sopt_ f = sopt' f Nothing false

soptR_ :: Char -> Argument
soptR_ f = sopt' f Nothing true

-- short hand to create an Long-Argument argument
lopt' :: String -> (Maybe OptionArgumentObj) -> IsRepeatable -> Argument
lopt' n a r = Option {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        Nothing
, repeatable: r
}

lopt :: String -> OptionArgumentObj -> Argument
lopt n a = lopt' n (Just a) false

loptR :: String -> OptionArgumentObj -> Argument
loptR n a = lopt' n (Just a) true

lopt_ :: String -> Argument
lopt_ n = lopt' n Nothing false

loptR_ :: String -> Argument
loptR_ n = lopt' n Nothing true

--------------------------------------------------------------------------------
-- Short hand option creation (with env tag)
--------------------------------------------------------------------------------

optE :: Char -> String -> OptionArgumentObj -> String -> Argument
optE f n a e = opt' (Just f) (Just n) (Just a) (Just e) false

optER :: Char -> String -> OptionArgumentObj -> String -> Argument
optER f n a e = opt' (Just f) (Just n) (Just a) (Just e) true

optE_ :: Char -> String -> String -> Argument
optE_ f n e = opt' (Just f) (Just n) Nothing (Just e) false

optER_ :: Char -> String -> String -> Argument
optER_ f n e = opt' (Just f) (Just n) Nothing (Just e) true

-- short hand to create an Short-Argument argument
soptE' :: Char -> (Maybe OptionArgumentObj) -> IsRepeatable -> String -> Argument
soptE' f a r e = Option {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        pure e
, repeatable: r
}

soptE :: Char -> OptionArgumentObj -> String -> Argument
soptE f a e = soptE' f (Just a) false e

soptER :: Char -> OptionArgumentObj -> String -> Argument
soptER f a e = soptE' f (Just a) true e

soptE_ :: Char -> String -> Argument
soptE_ f e = soptE' f Nothing false e

soptER_ :: Char -> String -> Argument
soptER_ f e = soptE' f Nothing true e

-- short hand to create an Long-Argument argument
loptE' :: String -> (Maybe OptionArgumentObj) -> IsRepeatable -> String -> Argument
loptE' n a r e = Option {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        pure e
, repeatable: r
}

loptE :: String -> OptionArgumentObj -> String -> Argument
loptE n a e = loptE' n (Just a) false e

loptER :: String -> OptionArgumentObj -> String -> Argument
loptER n a e = loptE' n (Just a) true e

loptE_ :: String -> String -> Argument
loptE_ n e = loptE' n Nothing false e

loptER_ :: String -> String -> Argument
loptER_ n e = loptE' n Nothing true e

-- short hand to create a Command
co :: String -> Argument
co n = Command { name: n, repeatable: false }

-- short hand to create a Positional argument
po :: String -> Argument
po n = Positional { name: n, repeatable: false }

poR :: String -> Argument
poR n = Positional { name: n, repeatable: true }

-- short hand to create a end-of-arguments marker
eoa :: Argument
eoa = EOA

-- short hand to create a group
gr :: Boolean -> IsRepeatable -> (Array (Array Argument)) -> Argument
gr b r xs = Group { optional: b, branches: (fromFoldable $ br <$> xs), repeatable: r }

-- short hand to create a optional group
gro :: IsRepeatable -> (Array (Array Argument)) -> Argument
gro = gr true

-- short hand to create a required group
grr :: IsRepeatable -> (Array (Array Argument)) -> Argument
grr = gr false

-- short hand to create a whole branch
br :: (Array Argument) -> Branch
br xs = fromFoldable xs

oa :: String -> Value -> OptionArgumentObj
oa n v =  { name: n
          , optional: false
          , default: Just v
          }

oa_ :: String -> OptionArgumentObj
oa_ n = { name:     n
        , optional: false
        , default:  Nothing
        }
