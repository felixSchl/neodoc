module Language.Docopt.Scanner (
    scan
  , Docopt(..)
  ) where

import Prelude
import Debug.Trace
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Data.Tuple
import Control.Apply ((*>), (<*))
import Text.Parsing.Parser.Combinators ((<?>))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(..), many, toList, fromList, (:), length, filter
                 , takeWhile, sort, head, catMaybes)
import qualified Data.Array as A
import Data.String (toLower, toCharArray, fromCharArray)
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import qualified Data.String as Str
import Data.Maybe
import Data.Either
import Language.Docopt.Parser.Base
import qualified Language.Docopt.Parser.Lexer as Lexer
import qualified Language.Docopt.Parser.Usage as Usage
import Text.Wrap (dedent)

type Docopt = { usage :: String, options :: List String }

section :: String -> Regex
section name
  = regex ("^([^\n]*" ++ name ++ "[^\n]*:(?:.*$)\n?(?:(?:[ \t].*)?(?:\n|$))*)")
          (Regex.parseFlags "gmi")

fixSection :: String -> String
fixSection section
  = Regex.replace'
      (regex ("(^[^:]+:)") (Regex.noFlags))
      (\m _ -> fromCharArray $ A.replicate (Str.length m) ' ')
      section


scan :: String -> Either P.ParseError Docopt
scan text = do
  u <- case sections "usage" of
              Nil        -> fail "No usage section found!"
              Cons x Nil -> return x
              _          -> fail "Multiple usage sections found!"

  return {
    usage:   fixSection u
  , options: fixSection <$> sections "options"
  }

  where
    fail msg = Left $ P.ParseError { message: msg
                                   , position: P.initialPos }

    sections n = maybe Nil
                       (catMaybes <<< toList)
                       (Regex.match (section n) text)
