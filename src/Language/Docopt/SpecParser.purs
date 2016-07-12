module Language.Docopt.SpecParser (
    module ReexportUsage
  , module ReexportDesc
  , parseDesc
  , parseUsage
  ) where

import Language.Docopt.SpecParser.Usage hiding (run, parse) as ReexportUsage
import Language.Docopt.SpecParser.Desc  hiding (run, parse) as ReexportDesc

import Language.Docopt.SpecParser.Usage as U
import Language.Docopt.SpecParser.Desc  as D

parseDesc  = D.run
parseUsage = U.run
