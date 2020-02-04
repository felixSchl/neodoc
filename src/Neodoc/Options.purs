module Neodoc.Options where

import Prelude (bind, ($), pure)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:?), (.!=))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Newtype
import Data.Maybe (Maybe(..))

import Neodoc.Env (Env)
import Neodoc.OptionAlias (OptionAlias)
import Neodoc.OptionAlias as OA


type Argv = Array String

type NeodocOptionsObj =
  { argv         :: Maybe Argv   -- ^ override argv. Defaults to `process.argv`
  , env          :: Maybe Env    -- ^ override env.  Defaults to `process.env`
  , optionsFirst :: Boolean      -- ^ enable "option-first"
  , dontExit     :: Boolean      -- ^ don't exit the process upon failure
  , smartOptions :: Boolean      -- ^ parse singleton groups as opts if possible
  , stopAt       :: Array String -- ^ stop parsing at these custom EOA markers
  , requireFlags :: Boolean      -- ^ do not ignore missing flags
  , laxPlacement :: Boolean      -- ^ allow positionals/commands to be appear anywhere
  , version      :: Maybe String -- ^ the version string to display
  , versionFlags :: Array OptionAlias -- ^ list of flags that trigger 'version'
  , helpFlags    :: Array OptionAlias -- ^ list of flags that trigger 'help'
  , repeatableOptions :: Boolean -- ^ options are always allowed to repeat
  , allowUnknown      :: Boolean -- ^ allow unknown options in the input
  -- , transforms ::
  --     { presolve :: Either
  --         (Array (Spec UsageLayout -> Aff (Spec UsageLayout)))
  --         (Array (Spec UsageLayout -> Either SolveError (Spec UsageLayout)))
  --     , postsolve :: Either
  --         (Array (Spec SolvedLayout -> Aff (Spec SolvedLayout)))
  --         (Array (Spec SolvedLayout -> Either SolveError (Spec SolvedLayout)))
  --     }
  }

newtype NeodocOptions = NeodocOptions NeodocOptionsObj

derive instance newtypeNeodocOptions :: Newtype NeodocOptions _

derive newtype instance encodeJsonNeodocOptions :: EncodeJson NeodocOptions


instance decodeJsonNeodocOptions :: DecodeJson NeodocOptions where
  decodeJson json = do
    obj <- decodeJson json

    argv              <- obj .:? "argv"
    env               <- obj .:? "env"
    version           <- obj .:? "version"

    allowUnknown      <- obj .:? "allowUnknown"      .!= doo.allowUnknown
    dontExit          <- obj .:? "dontExit"          .!= doo.dontExit
    helpFlags         <- obj .:? "helpFlags"         .!= doo.helpFlags
    laxPlacement      <- obj .:? "laxPlacement"      .!= doo.laxPlacement
    optionsFirst      <- obj .:? "optionsFirst"      .!= doo.optionsFirst
    repeatableOptions <- obj .:? "repeatableOptions" .!= doo.repeatableOptions
    requireFlags      <- obj .:? "requireFlags"      .!= doo.requireFlags
    smartOptions      <- obj .:? "smartOptions"      .!= doo.smartOptions
    stopAt            <- obj .:? "stopAt"            .!= doo.stopAt
    versionFlags      <- obj .:? "versionFlags"      .!= doo.versionFlags

    pure $ NeodocOptions
      { allowUnknown
      , argv
      , dontExit
      , env
      , helpFlags
      , laxPlacement
      , optionsFirst
      , repeatableOptions
      , requireFlags
      , smartOptions
      , stopAt
      , version
      , versionFlags
      }


defaultOptions :: NeodocOptions
defaultOptions = NeodocOptions defaultOptionsObj


defaultOptionsObj :: NeodocOptionsObj
defaultOptionsObj =
  { argv:         Nothing
  , env:          Nothing
  , optionsFirst: false
  , dontExit:     false
  , smartOptions: false
  , stopAt:       []
  , requireFlags: false
  , laxPlacement: false
  , version:      Nothing
  , versionFlags: [ OA.Long "version" ]
  , helpFlags:    [ OA.Short 'h', OA.Long "help"    ]
  , repeatableOptions: false
  , allowUnknown: false
  -- , transforms:   { presolve: Right [], postsolve: Right [] }
  }


-- Shorter name for DecodeJson instance
doo :: NeodocOptionsObj
doo = defaultOptionsObj


customize
  :: NeodocOptions
  -> (NeodocOptionsObj -> NeodocOptionsObj)
  -> NeodocOptions
customize (NeodocOptions o) f = NeodocOptions (f o)


optionsToJson :: NeodocOptions -> Json
optionsToJson = encodeJson


optionsFromJson :: Json -> Either String NeodocOptions
optionsFromJson = decodeJson
