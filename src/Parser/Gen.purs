module Docopt.Parser.Gen where

import Prelude
import Data.Either
import Control.Apply ((*>), (<*))
import Data.List (List(..), fromList, foldM, many)
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Docopt.Parser.Base
import Docopt.Parser.Usage

generateParser :: List Usage -> P.Parser String Unit
generateParser usages = do
  P.choice (generateUsageParser <$> usages)

  where
    generateUsageParser :: Usage -> P.Parser String Unit
    generateUsageParser (Usage name mutexes) = do
      P.string name
      P.choice (generateMutexParser <$> mutexes)
      P.eof

    generateMutexParser :: List UsageNode -> P.Parser String Unit
    generateMutexParser nodes = do
      foldM step unit nodes
      where
        step _ n = generateNodeParser n

    generateNodeParser :: UsageNode -> P.Parser String Unit
    generateNodeParser _ = P.fail "Node parser generation not implemented"
