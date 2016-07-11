module Language.Docopt.Scanner (
    scan
  , Docopt(..)
  ) where

import Prelude
import Text.Parsing.Parser (ParseError(ParseError)) as P
import Text.Parsing.Parser.Pos (initialPos) as P
import Data.List (List(Nil, Cons), fromFoldable, catMaybes)
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Data.String (length, trim) as String
import Data.String.Yarn (replicate) as String
import Data.Maybe (maybe)
import Data.Either (Either(Left), fromRight)
import Partial.Unsafe (unsafePartial)

type Docopt = {
  usage         :: String
, options       :: List String
, originalUsage :: String
}

section :: String -> Regex
section name = unsafePartial $ fromRight $
  regex ("^([^\n]*" <> name <> "[^\n]*:(?:.*$)\n?(?:(?:[ \t].*)?(?:\n|$))*)")
        (Regex.parseFlags "gmi")

fixSection :: String -> String
fixSection s
  = Regex.replace'
      (unsafePartial $ fromRight $ regex ("(^[^:]+:)") (Regex.noFlags))
      (\m _ -> String.replicate (String.length m) ' ')
      s


scan :: String -> Either P.ParseError Docopt
scan text = do
  u <- case sections "usage" of
              Nil        -> fail "No usage section found!"
              Cons x Nil -> pure x
              _          -> fail "Multiple usage sections found!"

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
