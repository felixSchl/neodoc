module Test.Support.Arguments where

import Prelude
import Data.Maybe
import Data.List (List(..), toList, length, fromList, singleton)
import Language.Docopt.Value
import Language.Docopt.Argument
import Language.Docopt.Argument.Option hiding (IsRepeatable)

-- short hand to create an Option argument
opt'
  :: Maybe Flag
  -> Maybe Name
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

opt :: Flag -> Name -> OptionArgumentObj -> Argument
opt f n a = opt' (Just f) (Just n) (Just a) Nothing false

optR :: Flag -> Name -> OptionArgumentObj -> Argument
optR f n a = opt' (Just f) (Just n) (Just a) Nothing true

opt_ :: Flag -> Name -> Argument
opt_ f n = opt' (Just f) (Just n) Nothing Nothing false

optR_ :: Flag -> Name -> Argument
optR_ f n = opt' (Just f) (Just n) Nothing Nothing true

-- short hand to create an Short-Argument argument
sopt' :: Flag -> (Maybe OptionArgumentObj) -> IsRepeatable -> Argument
sopt' f a r = Option {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        Nothing
, repeatable: r
}

sopt :: Flag -> OptionArgumentObj -> Argument
sopt f a = sopt' f (Just a) false

soptR :: Flag -> OptionArgumentObj -> Argument
soptR f a = sopt' f (Just a) true

sopt_ :: Flag -> Argument
sopt_ f = sopt' f Nothing false

soptR_ :: Flag -> Argument
soptR_ f = sopt' f Nothing true

-- short hand to create an Long-Argument argument
lopt' :: Name -> (Maybe OptionArgumentObj) -> IsRepeatable -> Argument
lopt' n a r = Option {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        Nothing
, repeatable: r
}

lopt :: Name -> OptionArgumentObj -> Argument
lopt n a = lopt' n (Just a) false

loptR :: Name -> OptionArgumentObj -> Argument
loptR n a = lopt' n (Just a) true

lopt_ :: Name -> Argument
lopt_ n = lopt' n Nothing false

loptR_ :: Name -> Argument
loptR_ n = lopt' n Nothing true

--------------------------------------------------------------------------------
-- Short hand option creation (with env tag)
--------------------------------------------------------------------------------

optE :: Flag -> Name -> OptionArgumentObj -> String -> Argument
optE f n a e = opt' (Just f) (Just n) (Just a) (Just e) false

optER :: Flag -> Name -> OptionArgumentObj -> String -> Argument
optER f n a e = opt' (Just f) (Just n) (Just a) (Just e) true

optE_ :: Flag -> Name -> String -> Argument
optE_ f n e = opt' (Just f) (Just n) Nothing (Just e) false

optER_ :: Flag -> Name -> String -> Argument
optER_ f n e = opt' (Just f) (Just n) Nothing (Just e) true

-- short hand to create an Short-Argument argument
soptE' :: Flag -> (Maybe OptionArgumentObj) -> IsRepeatable -> String -> Argument
soptE' f a r e = Option {
  flag:       pure f
, name:       Nothing
, arg:        a
, env:        pure e
, repeatable: r
}

soptE :: Flag -> OptionArgumentObj -> String -> Argument
soptE f a e = soptE' f (Just a) false e

soptER :: Flag -> OptionArgumentObj -> String -> Argument
soptER f a e = soptE' f (Just a) true e

soptE_ :: Flag -> String -> Argument
soptE_ f e = soptE' f Nothing false e

soptER_ :: Flag -> String -> Argument
soptER_ f e = soptE' f Nothing true e

-- short hand to create an Long-Argument argument
loptE' :: Name -> (Maybe OptionArgumentObj) -> IsRepeatable -> String -> Argument
loptE' n a r e = Option {
  flag:       Nothing
, name:       pure n
, arg:        a
, env:        pure e
, repeatable: r
}

loptE :: Name -> OptionArgumentObj -> String -> Argument
loptE n a e = loptE' n (Just a) false e

loptER :: Name -> OptionArgumentObj -> String -> Argument
loptER n a e = loptE' n (Just a) true e

loptE_ :: Name -> String -> Argument
loptE_ n e = loptE' n Nothing false e

loptER_ :: Name -> String -> Argument
loptER_ n e = loptE' n Nothing true e

-- short hand to create a Command
co :: String -> Argument
co n = Command n false

-- short hand to create a Positional argument
po :: String -> Argument
po n = Positional n false

poR :: String -> Argument
poR n = Positional n true

-- short hand to create a end-of-arguments marker
eoa :: Argument
eoa = EOA

-- short hand to create a group
gr :: Boolean -> IsRepeatable -> (Array (Array Argument)) -> Argument
gr b r xs = Group b (toList $ br <$> xs) r

-- short hand to create a optional group
gro :: IsRepeatable -> (Array (Array Argument)) -> Argument
gro = gr true

-- short hand to create a required group
grr :: IsRepeatable -> (Array (Array Argument)) -> Argument
grr = gr false

-- short hand to create a whole branch
br :: (Array Argument) -> Branch
br xs = toList xs

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
