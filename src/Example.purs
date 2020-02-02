module Example where

import Prelude (class Show, Unit, pure, show, ($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception
import Effect.Console (log)
import Neodoc.Options (NeodocOptions, customize, defaultOptions) as Neodoc
-- import Neodoc.Value as Neodoc


help :: String
help = """
usage: mkdir [-pv] [-m mode] <directory> ...
"""

opts :: Neodoc.NeodocOptions
opts = Neodoc.customize Neodoc.defaultOptions (_ {
  smartOptions = true
, optionsFirst = true
, repeatableOptions = true
})


newtype Args = Args {
  dirs :: Array String
, makeParents :: Boolean
}

derive instance genericArgs :: Generic Args _

instance showArgs :: Show Args where
  show = genericShow


getArgs :: ∀ e. (Show e) => Either e _ -> Effect Args
getArgs e = case e of
  Left e  -> throwException $ error $ show e
  Right v -> pure (Args v)


main :: Effect Unit
main = do
  log "todo"
  -- args <- Neodoc.runString help opts
  -- args' <- getArgs $ { dirs:_, makeParents:_ }
  --   <$> (Neodoc.fromValue =<< Neodoc.lookup' "<directory>" args)
  --   <*> (Neodoc.fromValue =<< Neodoc.lookup' "-p" args)
  -- log $ show args'
