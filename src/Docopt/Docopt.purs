-- |
-- | Docopt utiltiy surface.
-- |
-- | The impure part of docopt, providing conventient entry
-- | points and functions to use docopt.
-- |

module Docopt (
    run
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
import Data.List.WordsLines (lines, unlines)

import Language.Docopt (parseDocopt, evalDocopt)
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
type Options = {
  argv         :: Maybe Argv  -- ^ override argv. Defaults to `process.argv`
, env          :: Maybe Env   -- ^ override env.  Defaults to `process.env`
, optionsFirst :: Boolean     -- ^ enable "option-first"
, dontExit     :: Boolean     -- ^ don't exit the process upon failure
, smartOptions :: Boolean     -- ^ parse singleton groups as opts if possible
}

defaultOptions :: Options
defaultOptions = {
  argv:         Nothing
, env:          Nothing
, optionsFirst: false
, dontExit:     false
, smartOptions: false
}

-- |
-- | Run docopt on the given docopt text.
-- |
-- | This either succeeds with the key/value mappings or fails with a
-- | descriptive help message.
-- |
run :: forall e
     . String
    -> Options
    -> Eff (DocoptEff e) (StrMap Value)
run d o = do
  argv <- maybe (A.drop 2 <$> Process.argv) (return <<< id) o.argv
  env  <- maybe Process.getEnv              (return <<< id) o.env
  either onError return
         do
          { specification, usage } <- parseDocopt d o.smartOptions
          lmap ((help usage) ++ _) do
            evalDocopt specification env argv o.optionsFirst

  where
    onError e = do
      if not o.dontExit
        then do
          Console.log e
          Process.exit 1
        else
          throwException $ error $ e

    help usage
      = "Usage:\n"
          ++ (unlines $ ("  " ++ _) <$> lines (dedent usage))
          ++ "\n"
