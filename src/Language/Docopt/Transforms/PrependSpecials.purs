{-
# Prepend special arguments (help / version) to the spec.
-}

module Language.Docopt.Transforms.PrependSpecials where

import Prelude
import Data.List (List(Nil), singleton)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Language.Docopt.Specification (Specification)
import Language.Docopt.OptionAlias (OptionAlias)
import Language.Docopt.OptionAlias (OptionAlias(..)) as OptionAlias
import Language.Docopt.Argument (Argument(..))

prependSpecials :: List OptionAlias -> Specification -> Specification
prependSpecials aliases x =
  let g = Group {
            optional:   true
          , repeatable: true
          , branches:   singleton (toOption <$> aliases)
          }
   in prepend g <$> x
  where
    prepend _ x = x
    toOption alias = Group {
        optional: true
      , repeatable: true
      , branches: singleton $ singleton $ Option {
                      aliases:    alias :| Nil
                    , env:        Nothing
                    , repeatable: false
                    , arg:        Nothing
                    }
      }

