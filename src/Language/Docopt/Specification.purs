module Language.Docopt.Specification where

import Prelude
import Data.List (List(..), (:))
import Data.Foldable (intercalate, all)
import Language.Docopt.Usage
import Language.Docopt.Argument (prettyPrintBranch)

type Specification = List Usage

prettyPrintSpec :: Specification -> String
prettyPrintSpec branches =
  case branches of
    b:bs ->
      "usage: " <> intercalate "|" (prettyPrintBranch <$> b)
        <> (intercalate "\n" $ bs <#>
              \b' ->
                "    or: " <> intercalate "|" (prettyPrintBranch <$> b'))
    Nil  -> ""
