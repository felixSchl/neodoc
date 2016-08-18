-- |
-- | Docopt utility surface.
-- |
-- | The impure part of docopt, providing conventient entry
-- | points and functions to use docopt.
-- |

module Docopt (
    run
  , parse
  , defaultOptions
  , DocoptEff ()
  , Options (..)
  , Argv ()
  ) where

import Prelude
import Debug.Trace
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Control.Applicative (liftA1)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Foldable (any, intercalate)
import Data.Either (Either(..), either, fromRight)
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Node.FS (FS())
import Node.Process (PROCESS())
import Node.Process as Process
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Data.String.Regex as Regex
import Data.String.Regex (regex, Regex())
import Text.Wrap (dedent)
import Data.StrMap (StrMap())
import Data.Array as A
import Data.StrMap (member)
import Data.Bifunctor (lmap, bimap)
import Data.String.Yarn (lines, unlines)
import Data.String (trim) as String
import Language.Docopt.Errors (developerErrorMessage)
import Partial.Unsafe (unsafePartial)

import Language.Docopt (Docopt, parseDocopt, evalDocopt)
import Language.Docopt.Value (Value())
import Language.Docopt as D
import Language.Docopt.Env (Env())

type Argv = Array String
type DocoptEff e = ( process :: PROCESS
                   , err     :: EXCEPTION
                   , console :: CONSOLE
                   , fs      :: FS
                   | e
                   )

liftEffA :: ∀ e a. Eff (DocoptEff e) a -> Aff (DocoptEff e) a
liftEffA = liftEff

-- |
-- | Options for a docopt run
-- |

type ParseOptionsObj r = {
  smartOptions :: Boolean     -- ^ parse singleton groups as opts if possible
  | r
}

type Options r = {
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
}

defaultOptions :: Options {}
defaultOptions = {
  argv:         Nothing
, env:          Nothing
, optionsFirst: false
, dontExit:     false
, smartOptions: false
, stopAt:       []
, laxPlacement: false
, requireFlags: false
, version:      Nothing
, versionFlags: [ "--version" ]
, helpFlags:    [ "--help"    ]
}

-- |
-- | Parse the docopt specification from the given help text.
-- |
parse :: ∀ e r
       . String
      -> ParseOptionsObj r
      -> Eff (DocoptEff e) Docopt
parse helpText opts = do
  either (throwException <<< error) pure do
    parseDocopt helpText opts

data Action a
  = ShowHelp String
  | ShowVersion
  | Return a

-- |
-- | Run docopt on the given help text.
-- |
-- | This either succeeds with the key/value mappings or fails with a
-- | descriptive help message.
-- |
run :: ∀ e r
     . Either Docopt String
    -> Options r
    -> Eff (DocoptEff e) (StrMap Value)
run input opts = do
  argv <- maybe (A.drop 2 <$> Process.argv) pure opts.argv
  env  <- maybe Process.getEnv              pure opts.env

  program /\ action <- runEither do
    { program, specification, shortHelp, help } <- case input of
        (Left spec)   -> pure spec
        (Right help') -> parseDocopt help' opts

    bimap
      (fmtHelp program opts.helpFlags shortHelp)
      ((program /\ _) <<< case _ of
        output | canExit && output `has` opts.helpFlags    -> ShowHelp help
        output | canExit && output `has` opts.versionFlags -> ShowVersion
        output                                             -> Return output
      )
      (evalDocopt program specification env argv opts)

  case action of
    Return v      -> pure v
    ShowHelp help -> abort 0 (trimHelp help)
    ShowVersion   -> do
      mVer <- maybe readPkgVersion (pure <<< pure) opts.version
      case mVer of
        Just ver -> abort 0 ver
        Nothing  -> abort 1
          $ program
              <> ": version not detected."
              <> "\n"
              <> developerErrorMessage
      abort 0 ""

  where
    has x = any (_ `member` x)
    canExit = not opts.dontExit

    -- note: purescript needs the `a` for now:
    abort :: ∀ a. _ -> _ -> _ _ a
    abort _ msg | opts.dontExit = throwException (error msg)
    abort code msg
      = let log = if code == 0 then Console.log else Console.error
        in  do
          log msg
          Process.exit code

    runEither = flip either pure (abort 1)

    readPkgVersion = readPkgVersionImpl Just Nothing

    fmtHelp program helpFlags shortHelp errmsg
      = errmsg
      <> "\n"
      <> (dedent $ unlines $ ("  " <> _) <$> lines (dedent shortHelp))
      <> if A.length helpFlags == 0
          then ""
          else "\n" <> "See "
                        <> program <> " " <> (intercalate "/" helpFlags)
                        <> " for more information"

    trimHelp = Regex.replace (regex' "(^\\s*(\r\n|\n|\r))|((\r\n|\n|\r)\\s*$)" "g") ""
    regex' a b = unsafePartial $ fromRight $ regex a (Regex.parseFlags b)

foreign import readPkgVersionImpl
  :: ∀ e
   . (String -> Maybe String)
  -> Maybe String
  -> Eff e (Maybe String)
