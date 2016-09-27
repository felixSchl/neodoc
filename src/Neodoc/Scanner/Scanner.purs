module Neodoc.Scanner (
  scan
, Scan (..)
) where

import Prelude
import Data.Bifunctor (lmap)
import Text.Parsing.Parser (ParseError(ParseError)) as P
import Text.Parsing.Parser.Pos (initialPos) as P
import Data.List (List(Nil), (:), fromFoldable, catMaybes)
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Data.String (length, trim) as String
import Data.String.Yarn (replicate) as String
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(Left), fromRight)
import Partial.Unsafe (unsafePartial)
import Data.String.Regex.AnsiRegex (regex) as AnsiRegex
import Neodoc.Scanner.Error

type Scan = {
  usage         :: String
, options       :: List String
, originalUsage :: String
}

section :: String -> Regex
section name = unsafePartial $ fromRight $
  regex ("^([^\n]*" <> name <> "[^\n]*:(?:.*$)\n?(?:(?:[ \t].*)?(?:\n|$))*)")
        (Regex.parseFlags "gmi")

fixSection :: String -> String
fixSection = fixHeaders <<< removeEscapes
  where
    removeEscapes = to (Just ' ') AnsiRegex.regex
    fixHeaders    = to (Just ' ') $ unsafePartial
                                  $ fromRight
                                  $ regex "(^[^:]+:)" Regex.noFlags
    to c = flip Regex.replace' $ \m _ ->
              maybe "" (String.replicate (String.length m)) c


scan :: String -> Either ScanError Scan
scan text = lmap ScanError do
  u <- case sections "usage" of
              Nil   -> fail "No usage section found!"
              x:Nil -> pure x
              _     -> fail "Multiple usage sections found!"

  pure {
    usage:         fixSection u
  , options:       fixSection <$> sections "options"
  , originalUsage: String.trim u
  }

  where
    fail msg = Left $ P.ParseError msg P.initialPos true
    sections n = maybe Nil
                       (catMaybes <<< fromFoldable)
                       (Regex.match (section n) text)
