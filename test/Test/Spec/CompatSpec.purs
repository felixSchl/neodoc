module Test.Spec.CompatSpec (genCompatSpec) where

import Prelude
import Global (readFloat)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, later)
import Control.Monad.Trampoline (runTrampoline)
import Control.Alt ((<|>))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..), either)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Foldable (intercalate, for_)
import Text.Wrap (dedent)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.List (List, many, fromList)
import Test.Spec (Spec(), describe, it)
import Data.String (fromCharArray)
import Data.String as String
import Test.Support (vliftEff, runEitherEff)
import Text.Parsing.Parser.Pos (initialPos) as P
import Text.Parsing.Parser (runParser, runParserT, PState(..)) as P
import Text.Parsing.Parser.Combinators (manyTill, optional, between, sepBy,
                                       try, choice, (<?>), option) as P
import Text.Parsing.Parser.String (eof, string, anyChar, skipSpaces,
                                  char, noneOf) as P
import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Control.Apply ((*>), (<*))
import Data.Array as A
import Data.Int as Int
import Data.String.Argv as Argv

import Docopt as Docopt
import Language.Docopt (runDocopt)
import Language.Docopt.Value (Value(..), prettyPrintValue) as D
import Language.Docopt.Parser.Base (space, digit, alpha, upperAlpha, getInput)

newtype Test = Test {
  doc   :: String
, kases :: Array Kase
}

newtype Kase = Kase {
  out     :: Either String (List (Tuple String D.Value))
, options :: Docopt.Options {}
}

type Flags = {
  optionsFirst :: Boolean -- ^ 'p'
, smartOptions :: Boolean -- ^ 's'
}

parseFlags :: String -> Flags
parseFlags s = {
    optionsFirst: String.contains "p" s
  , smartOptions: String.contains "s" s
  }

parseUniversalDocoptTests :: forall eff
  . Eff (fs :: FS, err :: EXCEPTION | eff) (List Test)
parseUniversalDocoptTests = do
  f <- FS.readTextFile UTF8 "testcases.docopt"
  runEitherEff
    $ runTrampoline
      $ P.runParserT (P.PState { input: f, position: P.initialPos }) do
          many kase <* P.eof

  where
    comment = P.char '#' *> P.manyTill P.anyChar (P.char '\n')
    skipComments = void $ many comment

    kase = do
      P.skipSpaces *> skipComments *>  P.skipSpaces
      u  <- usage
      as <- A.many application
      return $ Test {
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
                        } $ parseFlags <$> do
                              P.char '/'
                              fromCharArray <$> A.many alpha
      many (P.char ' ')
      input <- Argv.parse <<< fromCharArray <<< fromList <$>
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
            return $ s
        ]
      P.skipSpaces *> skipComments *>  P.skipSpaces
      return $ Kase { out: output
                    , options: {
                        argv:         return input
                      , optionsFirst: flags.optionsFirst
                      , env:          return env
                      , dontExit:     true
                      , smartOptions: flags.smartOptions
                      , stopAt:       []
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
          return $ Tuple key val

        value = P.choice $ P.try <$> [
          D.BoolValue <$> do
            P.choice $ P.try <$> [
              P.string "false" *> return false
            , P.string "true"  *> return true
            ]
        , D.StringValue <$> do
            fromCharArray <$> do
              P.between (P.char '"') (P.char '"') (A.many $ P.noneOf ['"'])
        , D.ArrayValue <<< fromList <$> do
            P.between (P.char '[') (P.char ']') do
              P.skipSpaces
              flip P.sepBy (P.skipSpaces *> P.char ',' *> P.skipSpaces) do
                value
        , do
            si <- P.option 1 (P.char '-' *> return (-1))
            xs <- fromCharArray <$> A.some digit
            P.choice [
              D.FloatValue <<< ((Int.toNumber si) * _) <<< readFloat <$> do
                xss <- do
                  P.char '.'
                  fromCharArray <$> A.some digit
                return $ xs ++ "." ++ xss
            , return $ D.IntValue $ si * (fromJust $ Int.fromString xs)
            ]
        ]


    usage = do
      P.string "r\"\"\""
      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar $ P.string "\"\"\"\n"

-- Somehow, purescript needs this:
_liftEff :: forall e a. Eff e a -> Aff e a
_liftEff = liftEff

type CompatEff e = (fs :: FS, err :: EXCEPTION | e)

genCompatSpec :: forall e. Aff (CompatEff e) (Unit -> Spec (CompatEff e) Unit)
genCompatSpec = do
  tests <- _liftEff parseUniversalDocoptTests
  return $ \_ -> describe "Docopt compatibility" do
    for_ tests \(Test { doc, kases }) -> do
      describe (doc ++ "\n") do
        for_ kases \(Kase { options, out }) -> do
          let argv = fromJust options.argv
              env  = fromJust options.env
          describe (intercalate " " $
            (fromList $ StrMap.toList env <#> \t ->
              fst t ++ "=\"" ++ snd t ++ "\"")
            ++ argv
            ) do
            it ("\n" ++ prettyPrintOut out) do

              -- XXX: Manually break the execution context in order to avoid to
              --      avoid stack overflows by executing a large amount of Aff
              --      actions that run purely synchronous. Ideally, we would run
              --      the `Aff` action using it's `MonadRec` instance.
              -- Refer: https://github.com/owickstrom/purescript-spec/issues/24

              later (return unit)

              let result = runDocopt (dedent doc)
                                     (fromMaybe StrMap.empty options.env)
                                     argv
                                     options
              vliftEff $ case result of
                Left e ->
                  either
                    (\es ->
                      if es == "user-error"
                        then return unit
                        else if e == es
                          then return unit
                          else throwException $ error $
                            "Unexpected exception message: \"" ++ e ++ "\""
                    )
                    (const $ throwException $ error $ e)
                    out
                Right output -> do
                  either
                    (\_ -> do
                      throwException $ error $
                        "Unexpected output: \n"
                          ++ prettyPrintOut (pure $ StrMap.toList output)
                    )
                    (\expected ->
                      let actual = StrMap.toList output
                       in if (StrMap.fromFoldable expected /= output)
                        then throwException $ error $
                          "Unexpected output:\n"
                            ++ prettyPrintOut (pure actual)
                        else return unit)
                    out

  where
    prettyPrintOut :: Either String (List (Tuple String D.Value)) -> String
    prettyPrintOut (Left "user-error") = "fail"
    prettyPrintOut (Left err) = "fail with: \"" ++ err ++ "\""
    prettyPrintOut (Right xs)
      = intercalate "\n" $ xs <#> \(Tuple k v) -> k ++ " => " ++ show v
