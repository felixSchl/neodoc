module Language.Docopt.Errors where

import Prelude
import Data.List (List(Nil, Cons), reverse)
import Data.Generic (class Generic, gShow)
import Text.Wrap (dedent)
import Text.Parsing.Parser (ParseError(..)) as P
import Text.Parsing.Parser.Pos (Position) as P

mapWithIndex :: forall a b. (a -> Int -> b) -> List a -> List b
mapWithIndex f lst = reverse $ go 0 lst Nil
  where
  go _ Nil acc = acc
  go n (Cons x xs) acc = go (n+1) xs $ Cons (f x n) acc

--------------------------------------------------------------------------------
-- Errors (XXX: needs migration and improvement) -------------------------------
--------------------------------------------------------------------------------

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
  show (DocoptScanError        e) = "DocoptScanError "  <> show e
  show (DocoptUsageParseError  e) = "DocoptParseError " <> show e
  show (DocoptDescParseError   e) = "DocoptParseError " <> show e
  show (DocoptUserParseError _ e) = "DocoptParseError " <> show e
  show (DocoptSolveError       e) = "DocoptSolveError"  <> show e

unParseError :: P.ParseError -> { message  :: String
                                , fatal    :: Boolean
                                , position :: P.Position }
unParseError (P.ParseError message position fatal) = { message, position, fatal }

-- | XXX: It would be great to provide a link here to the project's website!
developerErrorMessage :: String
developerErrorMessage = dedent """
  This is an error with the program itself and not your fault.
  Please bring this to the program author's attention.
"""

prettyPrintDocoptError :: DocoptError -> String
prettyPrintDocoptError (DocoptScanError err) =
  "Failed to disect docopt text:"
    <> "\n"
    <> (unParseError err).message
    <> "\n"
    <> developerErrorMessage
prettyPrintDocoptError (DocoptUsageParseError err) =
  "Failed to parse the formal usage specification:"
    <> "\n"
    <> (unParseError err).message
    <> "\n"
    <> developerErrorMessage
prettyPrintDocoptError (DocoptDescParseError err) =
  "Failed to parse the option descriptions:"
    <> "\n"
    <> (unParseError err).message
    <> "\n"
    <> developerErrorMessage
prettyPrintDocoptError (DocoptSolveError (SolveError err)) =
  "Incoherent specification:"
    <> "\n"
    <> err
    <> "\n"
    <> developerErrorMessage
prettyPrintDocoptError
  (DocoptUserParseError
    argv (P.ParseError message _ _)) = message
