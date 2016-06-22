-- |
-- | Docopt utiltiy surface.
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
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Either (either)
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
import Data.Bifunctor (lmap)
import Data.String.Yarn (lines, unlines)

import Language.Docopt (Specification(), parseDocopt, evalDocopt)
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

liftEffA :: forall e a. Eff (DocoptEff e) a -> Aff (DocoptEff e) a
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
}

defaultOptions :: Options {}
defaultOptions = {
  argv:         Nothing
, env:          Nothing
, optionsFirst: false
, dontExit:     false
, smartOptions: false
, stopAt:       []
}

-- |
-- | Parse the docopt specification from the given help text.
-- |
parse :: forall e r
       . String
      -> ParseOptionsObj r
      -> Eff (DocoptEff e) Specification
parse helpText opts = do
  either (throwException <<< error) pure do
    { specification } <- parseDocopt helpText opts
    pure specification

-- |
-- | Run docopt on the given help text.
-- |
-- | This either succeeds with the key/value mappings or fails with a
-- | descriptive help message.
-- |
run :: forall e r
     . String
    -> Options r
    -> Eff (DocoptEff e) (StrMap Value)
run helpText opts = do
  argv <- maybe (A.drop 2 <$> Process.argv) (pure <<< id) opts.argv
  env  <- maybe Process.getEnv              (pure <<< id) opts.env
  either onError pure do
          { specification, usage } <- parseDocopt helpText opts
          lmap ((help usage) <> _) do
            evalDocopt specification env argv opts

  where
    onError e = do
      if not opts.dontExit
        then do
          Console.log e
          Process.exit 1
        else
          throwException $ error $ e

    help usage
      = dedent $ (unlines $ ("  " <> _) <$> lines (dedent usage)) <> "\n"
