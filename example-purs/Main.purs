module Main where

import Prelude
import Data.Generic
import Data.Either (Either(..))
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (log)
import Neodoc as Neodoc
import Neodoc.Options as Neodoc
import Neodoc.Value as Neodoc

help = """
usage: mkdir [-pv] [-m mode] <directory> ...
"""

opts = Neodoc.customize Neodoc.defaultOptions (_ {
  smartOptions = true
, optionsFirst = true
, repeatableOptions = true
})

newtype Args = Args {
  dirs :: Array String
, makeParents :: Boolean
}

derive instance genericArgs :: Generic Args
instance showArgs :: Show Args where
  show = gShow

getArgs :: ∀ e. (Show e) => Either e _ -> Eff _ Args
getArgs e = case e of
  Left e  -> throwException $ error $ show e
  Right v -> pure (Args v)

main = do
  args <- Neodoc.run help opts
  args' <- getArgs $ { dirs:_, makeParents:_ }
    <$> (Neodoc.fromValue =<< Neodoc.lookup' "<directory>" args)
    <*> (Neodoc.fromValue =<< Neodoc.lookup' "-p" args)
  log $ show args'
