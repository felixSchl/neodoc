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

  , Cache ()
  , CacheKey ()
  , CacheVal ()
  ) where

import Prelude
import Data.Function (on)
import Data.Tuple.Nested ((/\))
import Data.Map as Map
import Data.Map (Map())
import Data.List (List())
import Data.Tuple (Tuple())
import Language.Docopt.Env (Env())
import Text.Parsing.Parser (ParserT())
import Control.Monad.Transformerless.RWS (RWS())
import Language.Docopt.RichValue (RichValue())
import Language.Docopt.ArgParser.Token (PositionedToken(..))
import Language.Docopt.Argument (Argument(..), prettyPrintArg) as D
import Text.Parsing.Parser (PState(), Result()) as P
import Data.Foldable (intercalate)

type Parser a = ParserT (List PositionedToken)
                        (RWS Env Unit StateObj)
                        a
type ValueMapping = Tuple D.Argument RichValue

type Cache = Map CacheKey CacheVal
type CacheKey = Tuple    (List PositionedToken) (Required (Indexed D.Argument))
type CacheVal = P.Result (List PositionedToken) (List (Tuple D.Argument RichValue))

-- | The backing parser state.
type StateObj = {
  depth  :: Int     -- ^ the current tracked parse-depth
, done   :: Boolean -- ^ stop parsing now?
, failed :: Boolean -- ^ stop trying to recover now?
, cache  :: Cache   -- ^ the cache of previous parse results, given an input
}

--------------------------------------------------------------------------------
-- Auxiliary data types:
--------------------------------------------------------------------------------

-- | Auxiliary data structure for permutation parsing
data Required a = Required a | Optional a

instance eqRequired :: (Eq a) => Eq (Required a) where
  eq (Required a) (Required a') = a == a'
  eq (Optional a) (Optional a') = a == a'
  eq _            _             = false

-- XXX: this is provisionary
instance ordRequired :: (Ord a) => Ord (Required a) where
  compare (Required _) (Optional _) = GT
  compare (Optional _) (Required _) = LT
  compare a b = compare (unRequired a) (unRequired b)

instance showRequired :: (Show a) => Show (Required a) where
  show (Required a) = "Required " <> show a
  show (Optional a) = "Optional " <> show a

unRequired :: ∀ a. Required a -> a
unRequired (Required a) = a
unRequired (Optional a) = a

isRequired :: ∀ a. Required a -> Boolean
isRequired (Required _) = true
isRequired _            = false

toOptional :: ∀ a. Required a -> Required a
toOptional (Required a) = Optional a
toOptional (Optional a) = Optional a

prettyPrintRequiredIndexedArg :: Required (Indexed D.Argument) -> String
prettyPrintRequiredIndexedArg (Required (Indexed _ x)) = "Required " <> D.prettyPrintArg x
prettyPrintRequiredIndexedArg (Optional (Indexed _ x)) = "Optional " <> D.prettyPrintArg x

-- | Auxiliary data structure for permutation parsing to preserve the original
-- | order of arguments.
data Indexed a = Indexed Int a

instance eqIndexed :: (Eq a) => Eq (Indexed a) where
  eq (Indexed n a) (Indexed n' a') = n == n' && a == a'

instance showIndexed :: (Show a) => Show (Indexed a) where
  show (Indexed n a) = "Indexed " <> show n <> " " <> show a

-- | Note: use the tuple semantics for comparisons.
-- | Refer to the Ord instance of tuples for an explanation.
instance ordIndexed :: (Ord a) => Ord (Indexed a) where
  compare = compare `on` \(Indexed n a) -> n /\ a

getIndexedElem :: ∀ a. Indexed a -> a
getIndexedElem (Indexed _ x) = x

getIndex :: ∀ a. Indexed a -> Int
getIndex (Indexed ix _) = ix

-- | Auxiliary data structure to indiciate whether or not the contained elements
-- | are "fixed" in space or are freely interchangable in position.

data Clump a = Free a | Fixed a

instance showClump :: (Show a) => Show (Clump a) where
  show (Fixed a) = "Fixed " <> show a
  show (Free  a) = "Free "  <> show a

isFree :: ∀ a. Clump a -> Boolean
isFree (Free _) = true
isFree _        = false

prettyPrintArgClump :: Clump (List D.Argument) -> String
prettyPrintArgClump (Fixed xs) = "Fixed " <> (intercalate " " $ D.prettyPrintArg <$> xs)
prettyPrintArgClump (Free  xs) = "Free "  <> (intercalate " " $ D.prettyPrintArg <$> xs)
