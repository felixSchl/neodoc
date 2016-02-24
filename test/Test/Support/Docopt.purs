module Test.Support.Docopt where

import Prelude
import Data.Maybe (Maybe(..))
import Docopt
import Data.List (List(..), toList, length, fromList, singleton)

-- short hand to create a Command
co :: String -> Argument
co = Command

-- short hand to create a Positional argument
po :: String -> Boolean -> Argument
po = Positional

-- short hand to create a end-of-arguments marker
eoa :: Argument
eoa = EOA

-- short hand to create an Option argument
opt' :: Flag
      -> Name
      -> (Maybe OptionArgument)
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
    -> OptionArgument
    -> Argument
opt f n a = opt' f n (Just a) false

optR :: Flag
     -> Name
     -> OptionArgument
     -> Argument
optR f n a = opt' f n (Just a) true

-- short hand to create an Short-Option argument
sopt' :: Flag
      -> (Maybe OptionArgument)
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
    -> OptionArgument
    -> Argument
sopt f a = sopt' f (Just a) false

soptR :: Flag
     -> OptionArgument
     -> Argument
soptR f a = sopt' f (Just a) true

-- short hand to create an Long-Option argument
lopt' :: Name
      -> (Maybe OptionArgument)
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
    -> OptionArgument
    -> Argument
lopt n a = lopt' n (Just a) false

loptR :: Name
     -> OptionArgument
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

oa :: String -> Value -> OptionArgument
oa n v = OptionArgument n (Just v)

oa_ :: String -> OptionArgument
oa_ n = OptionArgument n Nothing

-- short hand for values
array = ArrayValue
str   = StringValue
bool  = BoolValue
