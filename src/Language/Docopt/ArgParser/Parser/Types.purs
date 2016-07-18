module Language.Docopt.ArgParser.Parser.Types (
    Parser ()
  , StateObj ()
  , ValueMapping ()

  , Required (..)
  , unRequired
  , isRequired
  , toOptional
  , prettyPrintRequiredIndexedArg

  , Indexed (..)
  , getIndexedElem
  , getIndex

  , Clump (..)
  , isFree
  , prettyPrintArgClump
  ) where

import Prelude
import Data.List (List())
import Data.Tuple (Tuple())
import Language.Docopt.Env (Env())
import Text.Parsing.Parser (ParserT())
import Control.Monad.Transformerless.RWS (RWS())
import Language.Docopt.RichValue (RichValue())
import Language.Docopt.ArgParser.Token (PositionedToken(..))
import Language.Docopt.Argument (Argument(..), prettyPrintArg) as D
import Data.Foldable (intercalate)

type Parser a = ParserT (List PositionedToken)
                        (RWS Env Unit StateObj)
                        a
type StateObj = { depth :: Int, done :: Boolean }
type ValueMapping = Tuple D.Argument RichValue

--------------------------------------------------------------------------------
-- Auxiliary data types:
--------------------------------------------------------------------------------

-- | Auxiliary data structure for permutation parsing
data Required a = Required a | Optional a

unRequired :: forall a. Required a -> a
unRequired (Required a) = a
unRequired (Optional a) = a

isRequired :: forall a. Required a -> Boolean
isRequired (Required _) = true
isRequired _            = false

toOptional :: forall a. Required a -> Required a
toOptional (Required a) = Optional a
toOptional (Optional a) = Optional a

prettyPrintRequiredIndexedArg :: Required (Indexed D.Argument) -> String
prettyPrintRequiredIndexedArg (Required (Indexed _ x)) = "Required " <> D.prettyPrintArg x
prettyPrintRequiredIndexedArg (Optional (Indexed _ x)) = "Optional " <> D.prettyPrintArg x

-- | Auxiliary data structure for permutation parsing to preserve the original
-- | order of arguments.
data Indexed a = Indexed Int a

getIndexedElem :: forall a. Indexed a -> a
getIndexedElem (Indexed _ x) = x

getIndex :: forall a. Indexed a -> Int
getIndex (Indexed ix _) = ix

-- | Auxiliary data structure to indiciate whether or not the contained elements
-- | are "fixed" in space or are freely interchangable in position.

data Clump a = Free a | Fixed a

instance showClump :: (Show a) => Show (Clump a) where
  show (Fixed a) = "Fixed " <> show a
  show (Free  a) = "Free "  <> show a

isFree :: forall a. Clump a -> Boolean
isFree (Free _) = true
isFree _        = false

prettyPrintArgClump :: Clump (List D.Argument) -> String
prettyPrintArgClump (Fixed xs) = "Fixed " <> (intercalate " " $ D.prettyPrintArg <$> xs)
prettyPrintArgClump (Free  xs) = "Free "  <> (intercalate " " $ D.prettyPrintArg <$> xs)
