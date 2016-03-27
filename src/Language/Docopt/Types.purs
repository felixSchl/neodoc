module Language.Docopt.Errors where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Function (on)
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar)
import Control.Apply ((*>))
import Data.Generic
import qualified Data.String as Str

import Language.Docopt.Value
import Language.Docopt.Argument
import qualified Language.Docopt.Option as O

--------------------------------------------------------------------------------
-- Errors (XXX: needs migration and improvement) -------------------------------
--------------------------------------------------------------------------------

import qualified Text.Parsing.Parser     as P
import qualified Text.Parsing.Parser.Pos as P

newtype SolveError = SolveError String

data DocoptError
  = DocoptScanError       P.ParseError
  | DocoptUsageParseError P.ParseError
  | DocoptDescParseError  P.ParseError
  | DocoptUserParseError  P.ParseError
  | DocoptSolveError      SolveError

derive instance genericSolveError :: Generic SolveError

instance showSolveError :: Show SolveError where
  show = gShow

instance showDocoptError :: Show DocoptError where
  show (DocoptScanError       e) = "DocoptScanError "  ++ show e
  show (DocoptUsageParseError e) = "DocoptParseError " ++ show e
  show (DocoptDescParseError  e) = "DocoptParseError " ++ show e
  show (DocoptUserParseError  e) = "DocoptParseError " ++ show e
  show (DocoptSolveError      e) = "DocoptSolveError"  ++ show e

unParseError :: forall r. P.ParseError -> { message :: String
                                          , position :: P.Position }
unParseError (P.ParseError e) = e

prettyPrintDocoptError :: DocoptError -> String
prettyPrintDocoptError (DocoptScanError err) =
  "Failed to disect docopt text. " ++ (unParseError err).message
prettyPrintDocoptError (DocoptUsageParseError err) =
  "Failed to parse the formal usage specification. "
  ++ (unParseError err).message
prettyPrintDocoptError (DocoptDescParseError err) =
  "Failed to parse the option descriptions. "
  ++ (unParseError err).message
prettyPrintDocoptError (DocoptSolveError err) =
  "Incoherent specification. " ++ show err
prettyPrintDocoptError (DocoptUserParseError err) = show err
  -- ""
  -- ++ (unParseError err).message
