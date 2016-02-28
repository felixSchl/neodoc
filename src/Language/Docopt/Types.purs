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

import qualified Text.Parsing.Parser as P

data DescriptionError
  = ArgumentMismatchError {
      option :: {
        flag :: Maybe O.Flag
      , name :: Maybe O.Name
      , arg  :: Maybe String
      }
    , description :: {
        arg :: Maybe String
      }
    }

data SolveError
  = DescriptionError DescriptionError

data DocoptError
  = DocoptScanError   P.ParseError
  | DocoptParseError  P.ParseError
  | DocoptSolveError  SolveError

derive instance genericSolveError       :: Generic SolveError
derive instance genericDescriptionError :: Generic DescriptionError

instance showSolveError :: Show SolveError where
  show = gShow

instance showDescriptionError :: Show DescriptionError where
  show = gShow

instance showDocoptError :: Show DocoptError where
  show (DocoptScanError err)  = "DocoptScanError "  ++ show err
  show (DocoptParseError err) = "DocoptParseError " ++ show err
  show (DocoptSolveError err) = "DocoptSolveError"  ++ show err
