module Test.Spec.DocoptSpec (docoptSpec) where

import Prelude
import Debug.Trace
import Global (readFloat, readInt)
import Control.Monad.Eff (Eff())
import Control.Monad.Trans (lift)
import Data.List (List(..), toList, concat, last, init)
import Text.Parsing.Parser as P
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.StrMap as StrMap
import Data.StrMap (StrMap())
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Text.Wrap (dedent)

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
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Plus (empty)
import Data.Array as A

parseUniversalDocoptTests :: forall eff
  . Eff (fs :: FS, err :: EXCEPTION | eff) Unit
parseUniversalDocoptTests = do
  f <- FS.readTextFile UTF8 "testcases.docopt"
  runEitherEff do
    P.runParser f parser
  return unit

  where
    parser = do
      many kase
      P.eof

    comment = do
      P.char '#'
      P.manyTill (P.anyChar) (P.char '\n')

    skipComments = do
      many comment
      pure unit

    kase = do
      P.skipSpaces *> skipComments *>  P.skipSpaces
      u  <- usage
      as <- many application
      pure unit
      -- traceShowA $ "Usage: " ++ (show u) ++ " - Cases: " ++ show as

    application = do
      P.skipSpaces *> skipComments *>  P.skipSpaces
      P.string "$ prog"
      input <- fromCharArray <<< fromList <$> do
                P.manyTill P.anyChar $ P.string "\n"
      P.skipSpaces *> skipComments *>  P.skipSpaces
      output <- P.choice $ P.try <$>
        [ Right <$> do
            P.between (P.char '{') (P.char '}') do
              flip P.sepBy (P.char ',') do
                P.skipSpaces
                key <- P.between (P.char '"') (P.char '"') do
                        fromCharArray <$> do
                          A.many $ P.noneOf [ '"' ]
                P.skipSpaces
                P.char ':'
                P.skipSpaces
                value
                return key
          , P.string "\"user-error\""
              *> many (P.char ' ')
              *> P.optional comment
              *> return (Left "user-error")
        ]
      P.skipSpaces *> skipComments *>  P.skipSpaces
      traceShowA $ "input: " ++ show input ++ " / output: " ++ show output
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
            xs  <- fromCharArray <$> A.many digit
            P.choice [
              readFloat <$> do
                xss <- do
                  P.char '.'
                  fromCharArray <$> A.many digit
                return $ xs ++ "." ++ xss
            , return $ readInt 10 xs
            ]
        ]


    usage = do
      P.string "r\"\"\""
      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar $ P.string "\"\"\"\n"


docoptSpec = \_ ->
  describe "Docopt" do
    it "..." do
      vliftEff do

        parseUniversalDocoptTests

        let env = StrMap.fromFoldable [
                    Tuple "FOO_OUTPUT" "BAR"
                  ]
        runEitherEff do
          output <- runDocopt env
            """
            Usage:
            """
            [ "push"
            -- , "-o", "~/foo/bar" (provide from env)
            , "-hhttp://localhost:5000"
            , "x"
            , "x", "y"
            , "--", "0", "1", "3"
            ]
          traceA (prettyPrintMap output show D.prettyPrintValue)
          return unit
