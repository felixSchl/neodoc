module Docopt.Extra (
  fromREADME
  ) where

import Prelude
import Node.FS.Aff (readTextFile)
import Control.Monad.Aff (Aff())
import Data.List (fromList)
import Data.Either (Either(..), either)
import Data.String (fromCharArray)
import Node.Path (FilePath())
import Node.FS (FS())
import Control.Alt ((<|>))
import Language.Docopt.Parser.Base (sof)
import Node.Encoding (Encoding(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)

import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P

fromREADME :: forall e
            . FilePath
           -> Aff (fs :: FS | e) String
fromREADME f = do
  c <- readTextFile UTF8 f
  either (throwError <<< error <<< show)
         return
         (P.runParser c parser)
  where
    parser :: P.Parser String String
    parser = do
      P.manyTill P.anyChar do
        (sof <|> (void $ P.char '\n'))
        P.string "```docopt"
        P.char '\n'
      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar do
          P.char '\n'
          P.string "```"

