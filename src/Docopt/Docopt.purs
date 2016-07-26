-- |
-- | Docopt utility surface.
-- |
-- | The impure part of docopt, providing conventient entry
-- | points and functions to use docopt.
-- |

module Docopt (
    run
  , parse
  , defaultOptions
  , DocoptEff ()
  , Options (..)
  , Argv ()
  ) where

import Prelude
import Debug.Trace
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Control.Applicative (liftA1)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Foldable (any)
import Data.Either (Either(..), either)
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..), maybe)
import Node.FS (FS())
import Node.Process (PROCESS())
import Node.Process as Process
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Text.Wrap (dedent)
import Data.StrMap (StrMap())
import Data.Array as A
import Data.StrMap (member)
import Data.Bifunctor (bimap)
import Data.String.Yarn (lines, unlines)

import Language.Docopt (Docopt, parseDocopt, evalDocopt)
import Language.Docopt.Value (Value())
import Language.Docopt as D
import Language.Docopt.Env (Env())

type Argv = Array String
type DocoptEff e = ( process :: PROCESS
                   , err     :: EXCEPTION
                   , console :: CONSOLE
                   , fs      :: FS
                   | e
                   )

liftEffA :: ∀ e a. Eff (DocoptEff e) a -> Aff (DocoptEff e) a
liftEffA = liftEff

-- |
-- | Options for a docopt run
-- |

type ParseOptionsObj r = {
  smartOptions :: Boolean     -- ^ parse singleton groups as opts if possible
  | r
}

type Options r = {
  argv         :: Maybe Argv   -- ^ override argv. Defaults to `process.argv`
, env          :: Maybe Env    -- ^ override env.  Defaults to `process.env`
, optionsFirst :: Boolean      -- ^ enable "option-first"
, dontExit     :: Boolean      -- ^ don't exit the process upon failure
, smartOptions :: Boolean      -- ^ parse singleton groups as opts if possible
, stopAt       :: Array String -- ^ stop parsing at these custom EOA markers
, requireFlags :: Boolean      -- ^ do not ignore missing flags
, laxPlacement :: Boolean      -- ^ allow positionals/commands to be appear anywhere
}

defaultOptions :: Options {}
defaultOptions = {
  argv:         Nothing
, env:          Nothing
, optionsFirst: false
, dontExit:     false
, smartOptions: false
, stopAt:       []
, laxPlacement: false
, requireFlags: false
}

-- |
-- | Parse the docopt specification from the given help text.
-- |
parse :: ∀ e r
       . String
      -> ParseOptionsObj r
      -> Eff (DocoptEff e) Docopt
parse helpText opts = do
  either (throwException <<< error) pure do
    parseDocopt helpText opts

-- |
-- | Run docopt on the given help text.
-- |
-- | This either succeeds with the key/value mappings or fails with a
-- | descriptive help message.
-- |
run :: ∀ e r
     . Either Docopt String
    -> Options r
    -> Eff (DocoptEff e) (StrMap Value)
run input opts = do
  argv <- maybe (A.drop 2 <$> Process.argv) pure opts.argv
  env  <- maybe Process.getEnv              pure opts.env

  output <- runEither do
    { program, specification, shortHelp, help } <- case input of
      (Left spec)   -> pure spec
      (Right help') -> parseDocopt help' opts

    bimap
      (fmtHelp shortHelp)
      (\output ->
        if any (flip member output) ["-?", "-h", "--help"]
          then Left  help
          else Right output
      )
      (evalDocopt program specification env argv opts)

  case output of
    Left help -> do
      Console.log help
      Process.exit 0
    Right vs -> pure vs

  where
    runEither = flip either pure \e ->
      if not opts.dontExit
        then do
          Console.error e
          Process.exit  1
        else throwException $ error $ e

    fmtHelp shortHelp errmsg
      = errmsg
      <> "\n"
      <> (dedent $ unlines $ ("  " <> _) <$> lines (dedent shortHelp))
