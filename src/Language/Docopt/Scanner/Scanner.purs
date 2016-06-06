module Language.Docopt.Scanner (
    scan
  , Docopt(..)
  ) where

import Prelude
import Text.Parsing.Parser (ParseError(ParseError)) as P
import Text.Parsing.Parser.Pos (initialPos) as P
import Data.List (List(Nil, Cons), toList, catMaybes)
import Data.Array as A
import Data.String (fromCharArray)
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Data.String as Str
import Data.Maybe (maybe)
import Data.Either (Either(Left))

type Docopt = {
  usage         :: String
, options       :: List String
, originalUsage :: String
}

section :: String -> Regex
section name
  = regex ("^([^\n]*" <> name <> "[^\n]*:(?:.*$)\n?(?:(?:[ \t].*)?(?:\n|$))*)")
          (Regex.parseFlags "gmi")

fixSection :: String -> String
fixSection s
  = Regex.replace'
      (regex ("(^[^:]+:)") (Regex.noFlags))
      (\m _ -> fromCharArray $ A.replicate (Str.length m) ' ')
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
  , originalUsage: u
  }

  where
    fail msg = Left $ P.ParseError { message:  msg
                                   , fatal:    true
                                   , position: P.initialPos }

    sections n = maybe Nil
                       (catMaybes <<< toList)
                       (Regex.match (section n) text)
