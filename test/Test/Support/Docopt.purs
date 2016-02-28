module Test.Support.Docopt where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..), toList, length, fromList, singleton)
import Language.Docopt.Value
import Language.Docopt.Argument
import qualified Language.Docopt.Option as O

-- short hand to create a Command
co :: String -> Argument
co = Command

-- short hand to create a Positional argument
po :: String -> Argument
po n = Positional n false

poR :: String -> Argument
poR n = Positional n true

-- short hand to create a end-of-arguments marker
eoa :: Argument
eoa = EOA

-- short hand to create an Option argument
opt' :: Flag
      -> Name
      -> (Maybe O.Argument)
      -> IsRepeatable
      -> Argument
opt' f n = Option (Just f) (Just n)

opt_ :: Flag
      -> Name
      -> Argument
opt_ f n = opt' f n Nothing false

optR_ :: Flag
      -> Name
      -> Argument
optR_ f n = opt' f n Nothing true

opt :: Flag
    -> Name
    -> O.Argument
    -> Argument
opt f n a = opt' f n (Just a) false

optR :: Flag
     -> Name
     -> O.Argument
     -> Argument
optR f n a = opt' f n (Just a) true

-- short hand to create an Short-Option argument
sopt' :: Flag
      -> (Maybe O.Argument)
      -> IsRepeatable
      -> Argument
sopt' f = Option (Just f) Nothing

sopt_ :: Flag
      -> Argument
sopt_ f = sopt' f Nothing false

soptR_ :: Flag
      -> Argument
soptR_ f = sopt' f Nothing true

sopt :: Flag
    -> O.Argument
    -> Argument
sopt f a = sopt' f (Just a) false

soptR :: Flag
     -> O.Argument
     -> Argument
soptR f a = sopt' f (Just a) true

-- short hand to create an Long-Option argument
lopt' :: Name
      -> (Maybe O.Argument)
      -> IsRepeatable
      -> Argument
lopt' n = Option Nothing (Just n)

lopt_ :: Name
      -> Argument
lopt_ n = lopt' n Nothing false

loptR_ :: Name
      -> Argument
loptR_ n = lopt' n Nothing true

lopt :: Name
    -> O.Argument
    -> Argument
lopt n a = lopt' n (Just a) false

loptR :: Name
     -> O.Argument
     -> Argument
loptR n a = lopt' n (Just a) true

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
br xs = Branch (toList xs)

oa :: String -> Value -> O.Argument
oa n v = O.Argument n (Just v)

oa_ :: String -> O.Argument
oa_ n = O.Argument n Nothing

-- short hand for values
array = ArrayValue
str   = StringValue
bool  = BoolValue
