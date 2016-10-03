module Test.Support.CompatParser (
    Test (..)
  , Kase (..)
  , Flags (..)
  , parseFlags
  , renderFlags
  , readTests
  ) where

import Prelude
import Global (readFloat)
import Data.Int as Int
import Data.String as String
import Data.StrMap as StrMap
import Data.String.Argv as Argv
import Data.String (fromCharArray)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Tuple (Tuple(..))
import Data.Array as A
import Data.List (List, many, toUnfoldable)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Alt ((<|>))
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Trampoline (runTrampoline)
import Partial.Unsafe (unsafePartial)

import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

import Text.Parsing.Parser.Pos (initialPos) as P
import Text.Parsing.Parser (runParser, runParserT, PState(..)) as P
import Text.Parsing.Parser.Combinators (manyTill, optional, between, sepBy,
                                       try, choice, (<?>), option) as P
import Text.Parsing.Parser.String (eof, string, anyChar, skipSpaces,
                                  char, noneOf) as P

import Neodoc as Neodoc
import Neodoc.Options
import Neodoc.Value (Value(..), prettyPrintValue) as D
import Neodoc.Spec.Parser.Base (space, digit, alpha, upperAlpha, getInput)

import Test.Support (runEitherEff)

newtype Test = Test {
  doc   :: String
, kases :: Array Kase
}

newtype Kase = Kase {
  out     :: Either String (List (Tuple String D.Value))
, options :: NeodocOptions
}

type Flags = {
  optionsFirst :: Boolean -- ^ 'p'
, smartOptions :: Boolean -- ^ 's'
, requireFlags :: Boolean -- ^ 'r'
, laxPlacement :: Boolean -- ^ 'l'
}

parseFlags :: String -> Flags
parseFlags s = {
  optionsFirst: String.contains "p" s
, smartOptions: String.contains "s" s
, requireFlags: String.contains "r" s
, laxPlacement: String.contains "l" s
}

renderFlags :: Flags -> String
renderFlags f = (if f.optionsFirst then "p" else "")
             <> (if f.smartOptions then "s" else "")
             <> (if f.requireFlags then "r" else "")
             <> (if f.laxPlacement then "l" else "")

readTests :: ∀ eff
   . String
  -> Eff (fs :: FS, err :: EXCEPTION | eff) (List Test)
readTests filepath = do
  f <- FS.readTextFile UTF8 filepath
  runEitherEff
    $ runTrampoline
      $ P.runParserT (P.PState f P.initialPos) do
          many kase <* P.eof

  where
    comment = P.char '#' *> P.manyTill P.anyChar (P.char '\n')
    skipComments = void $ many comment

    kase = do
      P.skipSpaces *> skipComments *>  P.skipSpaces
      u  <- usage
      as <- A.many application
      pure $ Test {
        doc: u
      , kases: as
      }

    application = do
      P.skipSpaces *> skipComments *>  P.skipSpaces
      P.char '$'
      env <- StrMap.fromFoldable <$> (many $ P.try do
        A.many (P.char ' ')
        envVar)
      many (P.char ' ')
      P.string "prog"
      flags <- P.option { optionsFirst: false
                        , smartOptions: false
                        , requireFlags: false
                        , laxPlacement: false
                        } $ parseFlags <$> do
                              P.char '/'
                              fromCharArray <$> A.many alpha
      many (P.char ' ')
      input <- Argv.parse <<< fromCharArray <<< toUnfoldable <$>
        P.manyTill (P.noneOf ['\n']) (P.char '\n')
      P.skipSpaces *> skipComments *>  P.skipSpaces
      output <- P.choice $ P.try <$>
        [ Right <$> do
            P.between (many space *> P.char '{' *> many space)
                      (many space *> P.char '}' *> many space) do
              flip P.sepBy (P.char ',') do
                P.skipSpaces
                key <- P.between (P.char '"') (P.char '"') do
                        fromCharArray <$> do
                          A.many $ P.noneOf [ '"' ]
                P.skipSpaces *> P.char ':' <* P.skipSpaces
                Tuple key <$> value
        , Left <$> do
            P.char '"'
            s <- fromCharArray <$> A.many do
                  P.try do
                    (P.char '\\' *> P.char '"') <|> P.noneOf ['"', '\n']
            P.char '"'
            many $ P.char ' '
            P.optional comment
            pure $ s
        ]
      P.skipSpaces *> skipComments *>  P.skipSpaces
      pure $ Kase { out: output
                    , options: NeodocOptions {
                        argv:         pure input
                      , optionsFirst: flags.optionsFirst
                      , env:          pure env
                      , dontExit:     true
                      , smartOptions: flags.smartOptions
                      , stopAt:       []
                      , requireFlags: flags.requireFlags
                      , laxPlacement: flags.laxPlacement
                      , version:      Nothing
                      , versionFlags: []
                      , helpFlags:    []
                      , transforms: {
                          presolve: Right []
                        , postsolve: Right []
                        }
                      }
                    }

      where
        envVar = do
          key <- fromCharArray <$> do
            A.many upperAlpha
          P.char '='
          val <- P.choice $ P.try <$> [
              D.prettyPrintValue <$> value
            , fromCharArray <$> (A.many $ P.noneOf [ ' ' ])
            ]
          pure $ Tuple key val

        value = P.choice $ P.try <$> [
          D.BoolValue <$> do
            P.choice $ P.try <$> [
              P.string "false" *> pure false
            , P.string "true"  *> pure true
            ]
        , D.StringValue <$> do
            fromCharArray <$> do
              P.between (P.char '"') (P.char '"') (A.many $ P.noneOf ['"'])
        , D.ArrayValue <<< toUnfoldable <$> do
            P.between (P.char '[') (P.char ']') do
              P.skipSpaces
              flip P.sepBy (P.skipSpaces *> P.char ',' *> P.skipSpaces) do
                value
        , do
            si <- P.option 1 (P.char '-' *> pure (-1))
            xs <- fromCharArray <$> A.some digit
            P.choice [
              D.FloatValue <<< ((Int.toNumber si) * _) <<< readFloat <$> do
                xss <- do
                  P.char '.'
                  fromCharArray <$> A.some digit
                pure $ xs <> "." <> xss
            , pure $ D.IntValue $ si * (unsafePartial $ fromJust $ Int.fromString xs)
            ]
        ]


    usage = do
      P.string "r\"\"\""
      fromCharArray <<< toUnfoldable <$> do
        P.manyTill P.anyChar $ P.string "\"\"\"\n"

