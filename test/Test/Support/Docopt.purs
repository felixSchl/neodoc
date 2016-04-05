module Test.Support.Docopt where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..), toList, length, fromList, singleton)
import Language.Docopt.Value
import Language.Docopt.Argument
import qualified Language.Docopt.Option as O

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
br xs = Branch (toList xs)

oa :: String -> Value -> O.Argument
oa n v = O.Argument { name: n
                    , optional: false
                    , default: Just v }

oa_ :: String -> O.Argument
oa_ n = O.Argument { name: n
                   , optional: false
                   , default: Nothing }

-- short hand for values
array = ArrayValue
str   = StringValue
bool  = BoolValue
int   = IntValue
