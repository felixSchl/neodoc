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
import Debug.Trace
import Control.Monad.Aff (Aff(), launchAff, liftEff')
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Either (Either(..), either)
import Node.Path (FilePath())
import Control.Monad.Eff (Eff())
import Node.FS.Aff (readTextFile)
import Data.String (fromCharArray)
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe)
import Node.FS (FS())
import Node.Process (PROCESS())
import Node.Process as Process
import Control.Monad.Eff.Class (liftEff)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Console as Console
import Text.Wrap (dedent)
import Data.Map (Map())
import Data.StrMap (StrMap())
import Data.Array (drop)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Foldable (intercalate)
import Data.List.WordsLines (lines, unlines)

import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P

import Language.Docopt (runDocopt, parseDocopt, applyDocopt)
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
}

defaultOptions :: Options
defaultOptions = {
  argv:         Nothing
, env:          Nothing
, optionsFirst: false
}

-- |
-- | Run docopt on the given docopt text.
-- |
run :: forall e
     . Options
    -> String
    -> Eff (DocoptEff e) (Either String (StrMap D.Value))
run o d = do
  argv <- maybe (A.drop 2 <$> Process.argv) (return <<< id) o.argv
  env  <- maybe Process.getEnv              (return <<< id) o.env
  return $ do
    { specification, usage } <- parseDocopt d
    lmap ((help usage) ++) do
      applyDocopt specification env argv o.optionsFirst

  where help usage = "Usage:\n"
            ++ (unlines $ ("  " ++) <$> lines (dedent usage))
            ++ "\n"
