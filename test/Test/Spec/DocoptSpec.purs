module Test.Spec.DocoptSpec (genDocoptSpec) where

import Prelude
import Debug.Trace
import Global (readFloat, readInt)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff(), liftEff')
import Control.Monad.Trans (lift)
import Data.List (List(..), toList, concat, last, init)
import Text.Parsing.Parser as P
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Map (Map(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Either (Either(..), either)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Foldable (foldl, for_, intercalate)
import Text.Wrap (dedent)
import Control.Monad.Aff (launchAff)

import Test.Assert (assert)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Assert.Simple
import Data.String (fromCharArray)

import Test.Support (vliftEff, runEitherEff, prettyPrintMap)
import Test.Support.Usage  as U
import Test.Support.Docopt as D
import Test.Support.Desc   as Desc

import Language.Docopt (runDocopt)
import Language.Docopt.Value as D

import Data.List (List(..), many, some, (:), toList, concat, singleton, length
                , fromList)
import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P

import Language.Docopt.Parser.Base
import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Data.Identity
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Plus (empty)
import Data.Array as A

newtype Test = Test {
  doc   :: String
, kases :: Array Kase
}

newtype Kase = Kase {
  argv :: List String
, out  :: Either String (List (Tuple String D.Value))
}

parseUniversalDocoptTests :: forall eff
  . Eff (fs :: FS, err :: EXCEPTION | eff) (List Test)
parseUniversalDocoptTests = do
  f <- FS.readTextFile UTF8 "testcases.docopt"
  runEitherEff $ P.runParser f do
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
      , kases: as <#> \(Tuple i o) -> Kase {
          argv: i
        , out:  o
        }
      }

    application = do
      P.skipSpaces *> skipComments *>  P.skipSpaces
      P.string "$ prog"
      P.skipSpaces
      input <- flip P.sepBy (P.char ' ') do
        many (P.char ' ')
        fromCharArray <$> do
          -- Note: Terminate on '{'. This is hacky and pragmatic,
          -- but it doesn't have to be any more than that...
          A.some $ P.noneOf [ ' ', '{', '\n', '"' ]
      P.skipSpaces *> skipComments *>  P.skipSpaces
      output <- P.choice $ P.try <$>
        [ Right <$> do
            P.between (P.char '{') (P.char '}') do
              flip P.sepBy (P.char ',') do
                P.skipSpaces
                key <- P.between (P.char '"') (P.char '"') do
                        fromCharArray <$> do
                          A.many $ P.noneOf [ '"' ]
                P.skipSpaces *> P.char ':' <* P.skipSpaces
                Tuple key <$> value
          , P.string "\"user-error\""
              *> many (P.char ' ')
              *> P.optional comment
              *> return (Left "user-error")
        ]
      P.skipSpaces *> skipComments *>  P.skipSpaces
      return $ Tuple input output

      where
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

          -- XXX: THIS IS WRONG (represent 'null'):
        , P.string "null" *> return (D.StringValue "null")

        , D.NumberValue <$> do
            xs  <- fromCharArray <$> A.some digit
            P.choice [
              readFloat <$> do
                xss <- do
                  P.char '.'
                  fromCharArray <$> A.some digit
                return $ xs ++ "." ++ xss
            , return $ readInt 10 xs
            ]
        ]


    usage = do
      P.string "r\"\"\""
      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar $ P.string "\"\"\"\n"

-- Somehow, purescript needs this:
_liftEff :: forall e a. Eff e a -> Aff e a
_liftEff = liftEff

genDocoptSpec = do
  tests <- _liftEff parseUniversalDocoptTests
  return $ \_ -> describe "Docopt" do
    for_ tests \(Test { doc, kases }) -> do
      describe (doc ++ "\n") do
        for_ kases \(Kase { argv, out }) -> do
          describe (intercalate " " argv) do
            it ("\n" ++ prettyPrintOut out) do
              let result = runDocopt StrMap.empty (dedent doc) (fromList argv)
              vliftEff $ case result of
                Left e ->
                  either
                    (const $ pure unit)
                    (const $ throwException $ error $ show e)
                    out
                Right r -> do
                  either
                    (\_ -> do
                      throwException $ error $
                        "Unexpected output: \n"
                          ++ prettyPrintOut (pure $ Map.toList r)
                    )
                    (\r' ->
                      let r'' = Map.toList r
                       in if (r'' /= r')
                        then throwException $ error $
                          "Unexpected output:\n"
                            ++ prettyPrintOut (pure r'')
                        else return unit)
                    out

  where
    prettyPrintOut :: Either String (List (Tuple String D.Value)) -> String
    prettyPrintOut (Left err) = "fail with " ++ show err
    prettyPrintOut (Right xs)
      = intercalate "\n" $ xs <#> \(Tuple k v) -> k ++ " => " ++ show v
