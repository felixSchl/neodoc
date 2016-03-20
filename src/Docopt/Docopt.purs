-- |
-- | Docopt utiltiy surface.
-- |
-- | The impure part of docopt, providing conventient entry
-- | points and functions to use docopt.
-- |

module Docopt (
    run
  , defaultOptions
  , fromREADME
  , fromREADME_
  ) where

import Prelude
import Debug.Trace
import Control.Monad.Aff (Aff(), launchAff, liftEff')
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Either (either)
import Node.Path (FilePath())
import Control.Monad.Eff (Eff())
import Node.FS.Aff (readTextFile)
import Data.String (fromCharArray)
import Node.Encoding (Encoding(..))
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Node.FS (FS())
import Node.Process (PROCESS())
import Node.Process as Process
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Language.Docopt.Parser.Base (sof)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Map (Map())

import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P

import Language.Docopt (runDocopt)
import Language.Docopt as D
import Language.Docopt.Env (Env())

type Argv = Array String
type DocoptEff e = ( process :: PROCESS
                   , err     :: EXCEPTION
                   , fs      :: FS
                   | e
                   )

liftEffA :: forall e a. Eff (DocoptEff e) a -> Aff (DocoptEff e) a
liftEffA = liftEff

-- |
-- | Options for a docopt run
-- |
type Options = {
  argv :: Maybe Argv -- ^ override argv. Defaults to `process.argv`
, env  :: Maybe Env  -- ^ override env.  Defaults to `process.env`
}

defaultOptions :: Options
defaultOptions = {
  argv: Nothing
, env:  Nothing
}

-- |
-- | Run docopt on the given docopt text.
-- |
run :: forall e
     . Options
    -> String
    -> Eff (DocoptEff e) (Either D.DocoptError (Map String D.Value))
run o d = do
  argv <- maybe Process.argv   (return <<< id) o.argv
  env  <- maybe Process.getEnv (return <<< id) o.env
  -- XXX: Print error here, if any
  return $ runDocopt d env argv

-- |
-- | Extract the docopt text from a README, then run it.
-- |
fromREADME :: forall e
           . Options
          -> FilePath
          -> Aff (DocoptEff e) (Either D.DocoptError (Map String D.Value))
fromREADME o f = do
  c <- readTextFile UTF8 f
  d <- either (throwError <<< error <<< show)
              return
              (P.runParser c parser)
  liftEffA $ run o d

  where
    parser :: P.Parser String String
    parser = do
      P.manyTill P.anyChar do
        (sof <|> (void $ P.char '\n'))
        P.string "```docopt"
        P.char '\n'
      fromCharArray <<< fromList <$> do
        P.manyTill P.anyChar do
          P.char '\n'
          P.string "```"

-- |
-- | Extract the docopt text from a README, then run it
-- | with the default options.
-- |
fromREADME_ :: forall e
             . FilePath
            -> Aff (DocoptEff e) (Either D.DocoptError (Map String D.Value))
fromREADME_ = fromREADME defaultOptions
