module Language.Docopt.Errors where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), reverse, toList)
import Data.Function (on)
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar, fromCharArray)
import Data.String.Ext (startsWith)
import Control.Apply ((*>))
import Data.Generic
import Data.Array as A
import Text.Wrap (dedent)
import qualified Data.String as Str

import Language.Docopt.Value
import Language.Docopt.Argument
import qualified Language.Docopt.Option as O

--------------------------------------------------------------------------------
-- XXX: Temporary polyfill until new version of `Data.List` is released.
--------------------------------------------------------------------------------

mapWithIndex :: forall a b. (a -> Int -> b) -> List a -> List b
mapWithIndex f lst = reverse $ go 0 lst Nil
  where
  go _ Nil acc = acc
  go n (Cons x xs) acc = go (n+1) xs $ Cons (f x n) acc

--------------------------------------------------------------------------------
-- Errors (XXX: needs migration and improvement) -------------------------------
--------------------------------------------------------------------------------

import qualified Text.Parsing.Parser     as P
import qualified Text.Parsing.Parser.Pos as P

type Argv = Array String
newtype SolveError = SolveError String

data DocoptError
  = DocoptScanError       P.ParseError
  | DocoptUsageParseError P.ParseError
  | DocoptDescParseError  P.ParseError
  | DocoptUserParseError  Argv P.ParseError
  | DocoptSolveError      SolveError

derive instance genericSolveError :: Generic SolveError

instance showSolveError :: Show SolveError where
  show = gShow

instance showDocoptError :: Show DocoptError where
  show (DocoptScanError        e) = "DocoptScanError "  ++ show e
  show (DocoptUsageParseError  e) = "DocoptParseError " ++ show e
  show (DocoptDescParseError   e) = "DocoptParseError " ++ show e
  show (DocoptUserParseError _ e) = "DocoptParseError " ++ show e
  show (DocoptSolveError       e) = "DocoptSolveError"  ++ show e

unParseError :: forall r. P.ParseError -> { message :: String
                                          , position :: P.Position }
unParseError (P.ParseError e) = e

developerErrorMessage :: String
developerErrorMessage = dedent """
  This is likely an error with the program itself and not your fault.
  Please raise this with the program's author.
"""

prettyPrintDocoptError :: DocoptError -> String
prettyPrintDocoptError (DocoptScanError err) =
  "Failed to disect docopt text:\n"
  ++ (unParseError err).message
  ++ "\n"
  ++ developerErrorMessage
prettyPrintDocoptError (DocoptUsageParseError err) =
  "Failed to parse the formal usage specification:\n"
  ++ (unParseError err).message
  ++ "\n"
  ++ developerErrorMessage
prettyPrintDocoptError (DocoptDescParseError err) =
  "Failed to parse the option descriptions:\n"
  ++ (unParseError err).message
  ++ "\n"
  ++ developerErrorMessage
prettyPrintDocoptError (DocoptSolveError (SolveError err)) =
  "Incoherent specification:\n"
  ++ err
  ++ "\n"
  ++ developerErrorMessage
prettyPrintDocoptError
  (DocoptUserParseError
    argv
    (P.ParseError { message: message }))
    = message
