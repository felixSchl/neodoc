-- |
-- | Docopt utiltiy surface.
-- |
-- | Extract docopt sources from a README, parse and run it.
-- |

module Docopt (
    fromREADME
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
import Data.Maybe (Maybe(..))
import Node.FS (FS())
import Node.Process (PROCESS())
import Node.Process as Process
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Language.Docopt.Parser.Base (sof)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff.Console (log, CONSOLE)

import Text.Parsing.Parser             as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos         as P
import Text.Parsing.Parser.String      as P

import Language.Docopt (runDocopt)

type DocoptEff = ( process :: PROCESS
                 , err     :: EXCEPTION
                 , fs      :: FS
                 )

liftEffA :: forall a. Eff DocoptEff a -> Aff DocoptEff a
liftEffA = liftEff

type Options = {
  argv :: Maybe (Array String) -- ^ override argv
}

defaultOptions :: Options
defaultOptions = {
  argv: Nothing
}

run :: forall e . Options -> String -> Eff DocoptEff Unit
run o d = do
  Process.argv
  pure unit

fromREADME :: forall e . Options -> FilePath -> Aff DocoptEff Unit
fromREADME o f = do
  c <- readTextFile UTF8 f
  d <- either (throwError <<< error <<< show)
              return
              (P.runParser c parser)
  u <- liftEffA $ run o d
  pure unit

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

fromREADME_ :: forall e . FilePath -> Aff DocoptEff Unit
fromREADME_ = fromREADME defaultOptions
