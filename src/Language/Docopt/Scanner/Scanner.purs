module Language.Docopt.Scanner (
    scan
  , Docopt(..)
  , Section()
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
                 , takeWhile, sort, head)
import qualified Data.Array as A
import Data.String (toLower, toCharArray, fromCharArray)
import qualified Data.String as Str
import Data.Maybe
import Data.Either
import Language.Docopt.Parser.Base
import qualified Language.Docopt.Parser.Lexer as Lexer
import qualified Language.Docopt.Parser.Usage as Usage
import Text.Wrap (dedent)

type Docopt = { usage :: Section, options :: List Section }
type Section = String

scan :: String -> Either P.ParseError Docopt
scan = (flip P.runParser docoptScanner) <<< dedent

(<??>) :: forall s a m. (Monad m) => String -> P.ParserT s m a -> P.ParserT s m a
(<??>) = flip (<?>)

docoptScanner :: P.Parser String Docopt
docoptScanner = do

  -- skim past anything uninteresting...
  P.manyTill P.anyChar ((void $ P.lookAhead sectionLabel) <|> P.eof)

  usage   <- usageSection
  options <- many optionSection

  return {
    usage:   usage
  , options: options
  }

  where
    -- | Parse a usage section, i.e.:
    -- | ```
    -- | Usage:
    -- |    program ARG <arg>... --bar=<qux>
    -- |    program naval fate
    -- | ```
    usageSection :: P.Parser String String
    usageSection = "usage section" <??> do
      label <- sectionLabel
      guard $ (toLower label) == "usage"
      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar
                    ((void $ P.lookAhead optionSection) <|> P.eof)


    optionSection :: P.Parser String String
    optionSection = "option section" <??> do
      label <- sectionLabel

      (guard $ endsWith "options" $ toLower label)
        <?> "section label ending in \"options\". E.g.: \"Advanved Options:\""

      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar
                    ((void $ P.lookAhead optionSection) <|> P.eof)

    -- | Parse a section label.
    -- | A label MUST be a piece of text, followed by a colon on the same line.
    -- | The text prior to the final colon makes up the label itself and can
    -- | subject to string comparisons. E.g. it is reasonable to expect "Usage:"
    -- | or "Options:", because both "usage" and "options" are clearly
    -- | identitifiable.
    -- |
    -- | In order to preserve the correct column position and offsets, the
    -- | parser, if successful, will modify the input stream to contain as many
    -- | spaces as it has consumed:
    -- |
    -- | ```
    -- | Options: -f, --foo
    -- |          -b, --bar
    -- | ```
    -- |
    -- | then becomes:
    -- |
    -- | ```
    -- |          -f, --foo
    -- |          -b, --bar
    -- | ```

    sectionLabel :: P.Parser String String
    sectionLabel = P.ParserT $ \(x@(P.PState { input: s, position: pos })) -> do
      P.unParserT (go s) x >>= \o ->
        return {
          consumed: o.consumed
        , input:    either (const s) (snd) o.result
        , result:   either Left (return <<< fst) o.result
        , position: pos
        }

      where
        go :: String -> P.Parser String (Tuple String String)
        go s = do
          w    <- many space
          name <- fromCharArray <$> (A.many $ P.noneOf [ '\n', ':' ])
          w'  <- many space
          P.char ':'
          w''  <- many space
          let len = length w + length w' + length w'' + Str.length name + 1
              fix = (fromCharArray $ A.replicate len ' ')
                ++ (Str.drop len s)
          return $ Tuple name fix

    endsWith :: String -> String -> Boolean
    endsWith sub s = Str.drop (Str.length s - Str.length sub) s == sub
