module Neodoc.Options where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Either (Either(..), fromRight)
import Data.Foreign (F, Foreign)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Control.Monad.Eff.Exception (Error, throwException, error, EXCEPTION)
import Control.Monad.Eff
import Neodoc.Spec
import Neodoc.Data.EmptyableLayout
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout
import Neodoc.Solve.Error
import Neodoc.Solve (SolveOptions)
import Neodoc.ArgParser.Options as ArgParser
import Neodoc.Env (Env(), unwrapEnv)
import Neodoc.SpecConversions (fromEmptyableSpec, toEmptyableSpec)
import Unsafe.Coerce

foreign import data JSCALLBACK :: !

type JsCallbackEff = (jsCallback :: JSCALLBACK, err :: EXCEPTION)

type Argv = Array String
newtype NeodocOptions = NeodocOptions {
  argv         :: Maybe Argv   -- ^ override argv. Defaults to `process.argv`
, env          :: Maybe Env    -- ^ override env.  Defaults to `process.env`
, optionsFirst :: Boolean      -- ^ enable "option-first"
, dontExit     :: Boolean      -- ^ don't exit the process upon failure
, smartOptions :: Boolean      -- ^ parse singleton groups as opts if possible
, stopAt       :: Array String -- ^ stop parsing at these custom EOA markers
, requireFlags :: Boolean      -- ^ do not ignore missing flags
, laxPlacement :: Boolean      -- ^ allow positionals/commands to be appear anywhere
, version      :: Maybe String -- ^ the version string to display
, versionFlags :: Array String -- ^ list of flags that trigger 'version'
, helpFlags    :: Array String -- ^ list of flags that trigger 'help'
, transforms   :: {
    presolve :: ∀ eff. Either
      (Array (Spec UsageLayout -> Eff JsCallbackEff (Spec UsageLayout)))
      (Array (Spec UsageLayout -> Either SolveError (Spec UsageLayout)))
  , postsolve :: ∀ eff. Either
      (Array (Spec SolvedLayout -> Eff JsCallbackEff (Spec SolvedLayout)))
      (Array (Spec SolvedLayout -> Either SolveError (Spec SolvedLayout)))
  }
}

defaultOptions :: NeodocOptions
defaultOptions = NeodocOptions _defaults

_defaults = {
  argv:         Nothing
, env:          Nothing
, optionsFirst: false
, dontExit:     false
, smartOptions: false
, stopAt:       []
, requireFlags: false
, laxPlacement: false
, version:      Nothing
, versionFlags: [ "--version" ]
, helpFlags:    [ "--help"    ]
, transforms:   { presolve: Right [], postsolve: Right [] }
}

instance isForeign :: IsForeign NeodocOptions where
  read v = NeodocOptions <$> do
    { argv:         _
    , env:          _
    , optionsFirst: _
    , dontExit:     _
    , smartOptions: _
    , stopAt:       _
    , requireFlags: _
    , laxPlacement: _
    , version:      _
    , versionFlags: _
    , helpFlags:    _
    , transforms:   _
    }
      <$> readArgv         v
      <*> readEnv          v
      <*> readOptionsFirst v
      <*> readDontExit     v
      <*> readSmartOptions v
      <*> readStopAt       v
      <*> readRequireFlags v
      <*> readLaxPlacement v
      <*> readVersion      v
      <*> readVersionFlags v
      <*> readHelpFlags    v
      <*> readTransforms   v

    where
    readArgv         = _maybe "argv"
    readEnv v        = (unwrapEnv <$> _) <$> F.readPropMaybe "env" v
    readOptionsFirst = _readBool "optionsFirst" _defaults.optionsFirst
    readDontExit     = _readBool "dontExit"     _defaults.dontExit
    readSmartOptions = _readBool "smartOptions" _defaults.smartOptions
    readRequireFlags = _readBool "requireFlags" _defaults.requireFlags
    readLaxPlacement = _readBool "laxPlacement" _defaults.laxPlacement
    readVersion      = _maybe    "version"
    readStopAt       = _default  "stopAt"       _defaults.stopAt
    readVersionFlags = _default  "versionFlags" _defaults.versionFlags
    readHelpFlags    = _default  "helpFlags"    _defaults.helpFlags
    readTransforms v = do
      transforms :: Foreign <- F.defaultIfUndefined "transforms" (F.toForeign {}) v
      { presolve: _, postsolve: _ }
        <$> readPresolveTransforms transforms
        <*> readPostsolveTransforms transforms
      where
      readPresolveTransforms v = do
        -- note: we trust these are functions for now.
        callbacks :: Array Foreign <- F.defaultIfUndefined "presolve" [] v
        pure $ Left $ callbacks <#> \fn ->
          \(spec :: Spec UsageLayout) ->
            let spec' = (unsafeCoerce fn) (F.write $ toEmptyableSpec spec)
             in case fromEmptyableSpec <$> (F.read spec' :: Either _ (Spec (EmptyableLayout UsageLayoutArg))) of
                  Left e  -> throwException $ error $ show e
                  Right s -> pure s
      readPostsolveTransforms v = do
        -- note: we trust these are functions for now.
        callbacks :: Array Foreign <- F.defaultIfUndefined "postsolve" [] v
        pure $ Left $ callbacks <#> \fn ->
          \(spec :: Spec SolvedLayout) ->
            let spec' = (unsafeCoerce fn) (F.write $ toEmptyableSpec spec)
             in case fromEmptyableSpec <$> (F.read spec' :: Either _ (Spec (EmptyableLayout SolvedLayoutArg))) of
                  Left e  -> throwException $ error $ show e
                  Right s -> pure s
    _maybe           = F.readPropMaybe
    _default         = F.defaultIfUndefined
    _readBool k d v  = F.isTruthy <$> _default k (F.truthy d) v
