-- |
-- | Docopt surface.
-- |
-- | The pure part of docopt, all functions are invariant.
-- |

module Language.Docopt (
    runDocopt
  , parseDocopt
  , evalDocopt
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

import Language.Docopt.Usage (Usage(..)) as D
import Language.Docopt.Errors (Argv, DocoptError(..), SolveError(..),
                              developerErrorMessage, prettyPrintDocoptError
                              ) as D
import Language.Docopt.Value (Value(..)) as D
import Language.Docopt.ParserGen  as G
import Language.Docopt.Trans.Flat as T

import Language.Docopt.Scanner      as Scanner
import Language.Docopt.Solver       as Solver
import Language.Docopt.Parser.Usage as Usage
import Language.Docopt.Parser.Desc  as Desc

type Docopt = {
  usage         :: String
, specification :: List D.Usage
}

type ParseOptsObj r = {
  smartOptions :: Boolean
  | r
}

type EvalOptsObj r = {
  optionsFirst :: Boolean
  | r
}

type Opts r = {
  smartOptions :: Boolean
, optionsFirst :: Boolean
  | r
}

data Origin
  = Argv
  | Environment
  | Default

-- |
-- | Parse the docopt text and produce a parser
-- | that can be applied to user input.
-- |
parseDocopt
  :: forall r
   .  String        -- ^ The docopt text
  -> ParseOptsObj r -- ^ Parse options
  -> Either String Docopt
parseDocopt docopt opts = do
  doc <- toScanErr       $ Scanner.scan $ dedent docopt
  us  <- toUsageParseErr $ Usage.run doc.usage opts.smartOptions
  ds  <- toDescParseErr  $ concat <$> Desc.run `traverse` doc.options
  prg <- toSolveErr      $ Solver.solve us ds
  pure $ { specification: prg , usage: doc.usage }

-- |
-- | Apply the generated docopt parser to user input.
-- |
evalDocopt
  :: forall r
   . List D.Usage  -- ^ The program specification
  -> StrMap String -- ^ The environment
  -> Array String  -- ^ The user input
  -> EvalOptsObj r -- ^ The eval opts
  -> Either String (StrMap D.Value)
evalDocopt prg env argv opts = do
  vs <- toUserParseErr argv
          $ G.runParser env argv
            $ G.genParser prg opts
  pure $ uncurry (T.reduce prg env) vs

-- |
-- | Parse the docopt source, derive a parser and then
-- | apply it to user input.
-- |
runDocopt
  :: forall r
   . String         -- ^ The docopt text
  -> StrMap String  -- ^ The environment
  -> Array String   -- ^ The user input
  -> Opts r         -- ^ Parse and eval opts
  -> Either String (StrMap D.Value)
runDocopt docopt env argv opts = do
  { specification } <- parseDocopt docopt opts
  evalDocopt specification env argv opts

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
