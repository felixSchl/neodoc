module Example.String where

import Prelude (Unit, show, ($))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, error)

import Neodoc (runString)
import Neodoc.Options (NeodocOptions(..), defaultOptionsObj)


exampleHelp :: String
exampleHelp = """
usage: git [--version] [--help] [-C <path>] [-c <name=value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [-p|--paginate|--no-pager] [--no-replace-objects] [--bare]
           [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
           <command> [<args>...]
"""


main :: Effect Unit
main = do
  let
    versionStr = "0.0.0-alpha"
    optionsString = NeodocOptions $ defaultOptionsObj
        { argv = Just ["--html-path", "test"]
        , version = Just versionStr
        }

  case runString exampleHelp optionsString (Just versionStr) of
    Left err -> error $ show err
    Right output -> log $ show output
