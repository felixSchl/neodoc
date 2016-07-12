-- |
-- | Docopt surface.
-- |
-- | The pure part of docopt, all functions are invariant.
-- |

module Language.Docopt (
    Docopt ()
  , runDocopt
  , preparseDocopt
  , parseDocopt
  , evalDocopt
  , module Language.Docopt.Specification
  , module Argument
  ) where

import Prelude
import Data.Either (Either)
import Data.List (List, concat)
import Data.StrMap (StrMap())
import Data.Tuple (uncurry)
import Data.Bifunctor (lmap)
import Data.Traversable (traverse)
import Text.Parsing.Parser as P
import Text.Wrap (dedent)

import Language.Docopt.Specification
import Language.Docopt.Usage (Usage) as D
import Language.Docopt.Errors (Argv, DocoptError(..), SolveError(..),
                              prettyPrintDocoptError
                              ) as D
import Language.Docopt.Value (Value()) as D
import Language.Docopt.ArgParser  as G
import Language.Docopt.Trans.Flat as T

import Language.Docopt.Argument     as Argument
import Language.Docopt.Scanner      as Scanner
import Language.Docopt.Solver       as Solver
import Language.Docopt.SpecParser.Usage as Usage
import Language.Docopt.SpecParser.Desc  as Desc

type Docopt = {
  program       :: String
, shortHelp     :: String
, specification :: Specification
}

type ParseOptionsObj r = {
  smartOptions :: Boolean
  | r
}

type EvalOptionsObj r = {
  optionsFirst :: Boolean
, stopAt       :: Array String
  | r
}

type Options r = {
  smartOptions :: Boolean
, optionsFirst :: Boolean
, stopAt       :: Array String
  | r
}

data Origin
  = Argv
  | Environment
  | Default

-- |
-- | Parse the docopt text and produce a parser that can be applied to user
-- | input.
-- |
preparseDocopt
  :: forall r
   .  String           -- ^ The docopt text
  -> ParseOptionsObj r -- ^ Parse options
  -> Either String { program      :: String
                   , usages       :: List Usage.Usage
                   , descriptions :: List Desc.Desc
                   }
preparseDocopt docopt options = do
  doc <- toScanErr       $ Scanner.scan $ dedent docopt
  u   <- toUsageParseErr $ Usage.run doc.usage options.smartOptions
  ds  <- toDescParseErr  $ concat <$> Desc.run `traverse` doc.options
  pure { descriptions: ds, usages: u.usages, program: u.program }

-- |
-- | Parse the docopt text and produce a parser that can be applied to user
-- | input.
-- |
parseDocopt
  :: forall r
   . String           -- ^ The neodoc text
  -> ParseOptionsObj r -- ^ Parse options
  -> Either String Docopt
parseDocopt helpText options = do
  doc <- toScanErr       $ Scanner.scan $ dedent helpText
  u   <- toUsageParseErr $ Usage.run doc.usage options.smartOptions
  ds  <- toDescParseErr  $ concat <$> Desc.run `traverse` doc.options
  prg <- toSolveErr      $ Solver.solve u.usages ds
  pure $ { specification: prg
         , shortHelp:     doc.originalUsage
         , program:       u.program
         }

-- |
-- | Apply the neodoc parser to user input.
-- |
evalDocopt
  :: forall r
   . Specification    -- ^ The program specification
  -> StrMap String    -- ^ The environment
  -> Array String     -- ^ The user input
  -> EvalOptionsObj r -- ^ The eval opts
  -> Either String (StrMap D.Value)
evalDocopt spec env argv options = do
  vs <- toUserParseErr argv $ G.run spec env argv options
  pure $ uncurry (T.reduce spec env) vs

-- |
-- | Parse the neodoc source, derive a parser and then apply it to user input.
-- |
runDocopt
  :: forall r
   . String         -- ^ The docopt text
  -> StrMap String  -- ^ The environment
  -> Array String   -- ^ The user input
  -> Options r      -- ^ Parse and eval options
  -> Either String (StrMap D.Value)
runDocopt docopt env argv options = do
  { specification } <- parseDocopt docopt options
  evalDocopt specification env argv options

toScanErr :: forall a. Either P.ParseError a -> Either String a
toScanErr  = lmap (D.prettyPrintDocoptError <<< D.DocoptScanError)

toUsageParseErr :: forall a. Either P.ParseError a -> Either String a
toUsageParseErr = lmap (D.prettyPrintDocoptError <<< D.DocoptUsageParseError)

toDescParseErr :: forall a. Either P.ParseError a -> Either String a
toDescParseErr = lmap (D.prettyPrintDocoptError <<< D.DocoptDescParseError)

toUserParseErr :: forall a. Array String -> Either P.ParseError a -> Either String a
toUserParseErr argv = lmap (D.prettyPrintDocoptError <<< D.DocoptUserParseError argv)

toSolveErr :: forall a. Either D.SolveError a -> Either String a
toSolveErr = lmap (D.prettyPrintDocoptError <<< D.DocoptSolveError)
