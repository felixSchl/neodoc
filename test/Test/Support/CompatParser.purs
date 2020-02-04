module Test.Support.CompatParser
  ( Test (..)
  , Kase (..)
  , Flags (..)
  , parseFlags
  , renderFlags
  , readTests
  )
where

import Prelude
  ( bind, discard, flip, negate, pure, void
  , ($), (*), (*>), (<$>), (<*), (<<<), (<>)
  )
import Control.Alt ((<|>))
import Control.Monad.Trampoline (runTrampoline)
import Data.Array as A
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List, many, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Argv as Argv
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect())
import Global (readFloat)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (fail, runParserT) as P
import Text.Parsing.Parser.Combinators
  (between, choice, manyTill, option, optional, sepBy, try) as P
import Text.Parsing.Parser.String
  (anyChar, char, eof, noneOf, skipSpaces, string) as P
import Test.Support (runEitherEff)

import Neodoc.Options
import Neodoc.Value (Value(..), prettyPrintValue) as D
import Neodoc.Spec.Parser.Base (alpha, digit, space, upperAlpha)



newtype Test = Test {
  doc   :: String
, kases :: Array Kase
}

newtype Kase = Kase {
  out     :: Either String (List (Tuple String D.Value))
, options :: NeodocOptions
}

type Flags = {
  optionsFirst      :: Boolean -- ^ 'p'
, smartOptions      :: Boolean -- ^ 's'
, requireFlags      :: Boolean -- ^ 'r'
, laxPlacement      :: Boolean -- ^ 'l'
, repeatableOptions :: Boolean -- ^ 'R'
, allowUnknown      :: Boolean -- ^ 'u'
}

parseFlags :: String -> Flags
parseFlags s = {
  optionsFirst:      String.contains (Pattern "p") s
, smartOptions:      String.contains (Pattern "s") s
, requireFlags:      String.contains (Pattern "r") s
, laxPlacement:      String.contains (Pattern "l") s
, repeatableOptions: String.contains (Pattern "R") s
, allowUnknown:      String.contains (Pattern "u") s
}

renderFlags :: Flags -> String
renderFlags f = (if f.optionsFirst then "p" else "")
             <> (if f.smartOptions then "s" else "")
             <> (if f.requireFlags then "r" else "")
             <> (if f.laxPlacement then "l" else "")
             <> (if f.repeatableOptions then "R" else "")
             <> (if f.allowUnknown then "u" else "")


readTests :: String -> Effect (List Test)
readTests filepath = do
  f <- readTextFile UTF8 filepath
  runEitherEff
    $ runTrampoline
      $ P.runParserT f do
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
      _ <- P.char '$'
      env <- Map.fromFoldable <$> (many $ P.try do
        _ <- A.many (P.char ' ')
        envVar)
      _ <- many (P.char ' ')
      _ <- P.string "prog"
      flags <- P.option { optionsFirst: false
                        , smartOptions: false
                        , requireFlags: false
                        , laxPlacement: false
                        , repeatableOptions: false
                        , allowUnknown: false
                        } $ parseFlags <$> do
                              _ <- P.char '/'
                              fromCharArray <$> A.many alpha
      _ <- many (P.char ' ')
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
                _ <- P.skipSpaces *> P.char ':' <* P.skipSpaces
                Tuple key <$> value
        , Left <$> do
            _ <- P.char '"'
            s <- fromCharArray <$> A.many do
                  P.try do
                    (P.char '\\' *> P.char '"') <|> P.noneOf ['"', '\n']
            _ <- P.char '"'
            _ <- many $ P.char ' '
            P.optional comment
            pure $ s
        ]
      P.skipSpaces *> skipComments *>  P.skipSpaces
      pure $ Kase { out: output
                  , options: NeodocOptions $ defaultOptionsObj {
                      argv              = pure input
                    , optionsFirst      = flags.optionsFirst
                    , env               = pure env
                    , dontExit          = true
                    , smartOptions      = flags.smartOptions
                    , requireFlags      = flags.requireFlags
                    , laxPlacement      = flags.laxPlacement
                    , repeatableOptions = flags.repeatableOptions
                    , allowUnknown      = flags.allowUnknown
                    }
                  }

      where
        envVar = do
          key <- fromCharArray <$> do
            A.many upperAlpha
          _ <- P.char '='
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
                  _ <- P.char '.'
                  fromCharArray <$> A.some digit
                pure $ xs <> "." <> xss
            , case Int.fromString xs of
                Just v ->
                  pure $ D.IntValue $ si * v
                Nothing ->
                  P.fail "Value not a valid Int"
            ]
        ]

    usage = do
      _ <- P.string "r\"\"\""
      fromCharArray <<< toUnfoldable <$> do
        P.manyTill P.anyChar $ P.string "\"\"\"\n"
