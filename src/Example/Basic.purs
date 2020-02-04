module Example.Basic where

import Prelude
  (class Show, Unit, pure, show, ($), (<$>), (<*>), (=<<), bind, identity)

import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception

import Neodoc (runString, lookup', Output(..))
import Neodoc.Error (NeodocError)
import Neodoc.Options (NeodocOptions, customize, defaultOptions) as Neodoc
import Neodoc.Value (fromValue)


help :: String
help = """
usage: mkdir [-pv] [-m mode] <directory> ...
"""

opts :: Neodoc.NeodocOptions
opts = Neodoc.customize Neodoc.defaultOptions (_
  { smartOptions = true
  , optionsFirst = true
  , repeatableOptions = true
  })


newtype Args = Args
  { dirs :: Array String
  , makeParents :: Boolean
  }

derive instance genericArgs :: Generic Args _

instance showArgs :: Show Args where
  show = genericShow


getArgs
  :: ∀ e. (Show e)
  => Either e { dirs :: Array String, makeParents :: Boolean }
  -> Effect Args
getArgs e = case e of
  Left err -> throwException $ error $ show err
  Right v -> pure (Args v)


main :: Effect Unit
main = do
  let
    args :: Either NeodocError Output
    args = runString help opts Nothing

    fixedArgs :: Output
    fixedArgs = either (\_ -> Output (Map.fromFoldable [])) identity args

    effectArgs :: Effect Args
    effectArgs = getArgs $ { dirs:_, makeParents:_ }
            <$> (fromValue =<< lookup' "<directory>" fixedArgs)
            <*> (fromValue =<< lookup' "-p" fixedArgs)

  processedArgs <- effectArgs
  log $ show (processedArgs :: Args)
