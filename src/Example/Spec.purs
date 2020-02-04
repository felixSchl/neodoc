module Example.Spec where

import Prelude (Unit, show, ($))

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Effect (Effect)
import Effect.Class.Console (log, error)

import Neodoc (runSpec)
import Neodoc.Data.Description (Description)
import Neodoc.Data.Layout (Layout(..))
import Neodoc.Data.UsageLayout (UsageLayout, UsageLayoutArg(..))
import Neodoc.Options (NeodocOptions(..), defaultOptionsObj)
import Neodoc.Spec


exampleSpec :: Spec UsageLayout
exampleSpec = Spec
  { program: "test"
  , layouts: NonEmpty.singleton $
      List.fromFoldable [
        NonEmpty.singleton $ Elem Stdin
      ]
  , descriptions: empty :: List Description
  , helpText: "test <command>"
  , shortHelp: "test <command>"
  }


main :: Effect Unit
main = do
  let
    versionStr = "0.0.0-alpha"
    optionsSpec = NeodocOptions $ defaultOptionsObj
        { argv = Just ["-"]
        , version = Just versionStr
        }

  case runSpec exampleSpec optionsSpec (Just versionStr) of
    Left err -> error $ show err
    Right output -> log $ show output
