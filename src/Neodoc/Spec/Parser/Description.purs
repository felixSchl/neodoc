module Neodoc.Spec.Parser.Description where

import Prelude
import Debug.Profile
import Debug.Trace
import Data.Pretty
import Data.Bifunctor (lmap)
import Data.Tuple.Nested ((/\))
import Data.Tuple (swap) as Tuple
import Data.NonEmpty ((:|))
import Data.Functor (($>))
import Data.Function (on)
import Data.Foldable (intercalate, foldl, elem)
import Control.Lazy (defer)
import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.MonadPlus (guard)
import Data.String (Pattern(..))
import Data.String as String
import Data.List (
  List(..), (:), many, some, head, length, filter, catMaybes, reverse,
  singleton)

import Neodoc.Parsing.Parser as P
import Neodoc.Parsing.Parser.Combinators ((<?>), (<??>))
import Neodoc.Parsing.Parser.Combinators as P
import Neodoc.Spec.Error
import Neodoc.Spec.Token as P
import Neodoc.Spec.TokenParser as P
import Neodoc.Spec.Parser.Combinators as P

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Nothing, Just), isJust, isNothing, maybe, fromMaybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Array as A
import Data.String.Ext ((^=))
import Data.Pretty (class Pretty, pretty)
import Partial.Unsafe
import Neodoc.Value
import Neodoc.Value as Value
import Neodoc.Spec.Lexer as L
import Neodoc.OptionAlias as OptionAlias
import Neodoc.OptionAlias (Aliases)
import Neodoc.Data.OptionArgument
import Neodoc.Data.Description

data Content
  = Default String
  | Env     String

instance showContent :: Show Content where
  show (Default s) = "Default " <> show s
  show (Env s) = "Env " <> show s

parse
  :: List P.PositionedToken
  -> Either SpecParseError (List Description)
parse toks = profileS "spec-parser::parse-desc" \_->
 lmap SpecParseError $ P.runTokenParser toks do
  P.markIndent do
    reverse <$> go Nil
    <* P.eof

  where
  go vs = do
    v <- (Just <$> desc) <|> (descContent true $> Nothing)
    case v of
      Just v' -> go (v' : vs)
      Nothing ->
        P.choice [
          desc >>= \v' -> go (v' : vs)
        , pure vs
        ]

  anyName :: P.TokenParser String
  anyName = P.angleName <|> P.shoutName <|> P.name

  desc :: P.TokenParser Description
  desc = defer \_-> "--option or <positional> description" P.<??> do
          P.choice $  [ optionDesc
                      , positionalsDesc
                      ]

  descContent :: Boolean -> P.TokenParser (List Content)
  descContent toplevel = do
    P.markIndent do
      catMaybes <$> (flip P.manyTill descEnd do
        P.choice $ P.try <$> [
          Just <<< Default <$> P.tag "default"
        , Just <<< Env     <$> P.tag "env"
        , P.anyToken $> Nothing
        ])
    <* (void P.eof <|> void (some P.newline))
    where
      descEnd = do
        P.choice [
          P.eof
        , void $ P.lookAhead do
            P.newline
            when (not toplevel) P.lessIndented
            P.choice [
              void P.sopt
            , void P.lopt
            , void P.angleName
            , void P.shoutName
            ]
        ]

  positionalsDesc :: P.TokenParser Description
  positionalsDesc = do
    P.angleName <|> P.shoutName
    P.option false $ P.tripleDot $> true
    descContent false
    pure CommandDescription

  optionDesc :: P.TokenParser Description
  optionDesc = do
    { aliases, arg, repeatable } <- opt
    description <- descContent false

    let defaults = getDefaultValue <$> filter isDefaultTag description
        envs     = getEnvKey       <$> filter isEnvTag     description
        default  = head defaults >>= id
        env      = head envs     >>= id

    when (length defaults > 1) do
      P.fatal $
        "Option " <> (intercalate ", " $ pretty <$> aliases)
                  <> " has multiple defaults!"

    when (length envs > 1) do
      P.fatal $
        "Option " <> (intercalate ", " $ pretty <$> aliases)
                  <> " has multiple environment mappings!"

    when (isJust default && isNothing arg) do
      P.fatal $
        "Option " <> (intercalate ", " $ pretty <$> aliases)
                  <> " does not take arguments. "
                  <> "Cannot specify defaults."

    pure $ OptionDescription
      aliases
      repeatable
      (do { name, optional } <- arg
          pure (OptionArgument name optional))
      default
      env

    where

    opt :: P.TokenParser _
    opt = do
      let optsP = "option" P.<??> P.choice [ P.try short, long ]
      xs <- optsP `P.sepBy1` do  -- extra options: [, -f]
        P.optional do
          P.comma
            *> many P.newline
            *> P.indented

      -- check integrity of the option-aliases
      aliases <- foldl (\acc next -> do
        cur <- acc
        if next `elem` cur
          then P.fatal $ "Option appears multiple times: " <> pretty next
          else pure $ cur <> singleton next
      ) (pure Nil) (_.alias <$> xs)

      -- check integrity of the option-arguments
      arg <- foldl (\acc next -> do
        cur <- acc
        case cur of
          Nothing  -> pure (Just next)
          Just arg ->
            if arg.name ^/=^ next.name
              then P.fatal $
                "Option-arguments mismatch: "
                  <> (show $ prettyAdhocOptArg arg)
                  <> " and "
                  <> (show $ prettyAdhocOptArg next)
              else pure (Just $ arg {
                          -- if any argument is shown as optional, they all
                          -- are considered optional in the eyes of the law.
                          optional = arg.optional || next.optional
                        })
      ) (pure Nothing) (catMaybes $ _.arg <$> xs)

      let repeatable = foldl (||) false (_.repeatable <$> xs)

      -- Note: this can safely be `unsafePartial` because we're using the
      -- `sepBy1` combinator above, which guarantees at least one match. It
      -- would be interesting to use the `NonEmpty` type for `sepBy1`.
      pure $ unsafePartial $ case aliases of
        (x : xs) -> {
          aliases:    x :| xs
        , arg:        arg
        , env:        Nothing
        , repeatable: repeatable
        }

      where
      prettyAdhocOptArg { optional: o, name: n }
        = (if o then "[" else "") <> n <> (if o then "]" else "")


    short :: P.TokenParser _
    short = do
      { flag, arg } <- do
        { chars: c :| cs, arg } <- P.sopt
        (guard $ A.length cs == 0) P.<?> "No stacked options"
        pure { flag: c, arg: arg }

      -- Grab the adjacent positional-looking argument
      -- in case the token did not have an explicit
      -- binding via `=`.
      arg <- maybe
              (P.optionMaybe do
                c <- P.optionMaybe $ P.choice [
                      P.lparen  $> (P.rparen  $> false)
                    , P.lsquare $> (P.rsquare $> true)
                    ]
                name <- P.shoutName <|> P.angleName
                optional <- fromMaybe (pure false) c
                pure { name, optional }
              )
              (pure <<< Just)
              arg

      repeatable <- P.option false $ P.tripleDot $> true

      pure {
        alias: OptionAlias.Short flag
      , arg
      , repeatable
      }

    long :: P.TokenParser _
    long = do
      { name, arg } <- P.lopt

      -- Grab the adjacent positional-looking argument
      -- in case the token did not have an explicit
      -- binding via `=`.
      arg' <- maybe
              (P.optionMaybe do
                c <- P.optionMaybe (P.choice [
                      P.lparen  $> (P.rparen  $> false)
                    , P.lsquare $> (P.rsquare $> true)
                    ])
                n <- P.shoutName <|> P.angleName
                optional <- fromMaybe (pure false) c
                pure { name: n, optional: optional }
              )
              (pure <<< Just)
              arg

      repeatable <- P.option false $ P.tripleDot $> true

      pure {
        alias: OptionAlias.Long name
      , arg:   arg'
      , repeatable
      }

isDefaultTag :: Content -> Boolean
isDefaultTag (Default _) = true
isDefaultTag _           = false

getDefaultValue :: Content -> Maybe Value
getDefaultValue (Default v) = either (const Nothing) Just (Value.parse v true)
getDefaultValue _           = Nothing

isEnvTag :: Content -> Boolean
isEnvTag (Env _) = true
isEnvTag _       = false

getEnvKey :: Content -> Maybe String
getEnvKey (Env k) = Just k
getEnvKey _       = Nothing


-- XXX: This is duplicated from Solver.purs.
--      Where should this live???
posArgsEq :: String -> String -> Boolean
posArgsEq = eq `on` (String.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^

notPosArgsEq :: String -> String -> Boolean
notPosArgsEq = not <<< posArgsEq
infixl 9 notPosArgsEq as ^/=^

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (String.stripPrefix (Pattern "<") s)
  stripSuffix s = fromMaybe s (String.stripSuffix (Pattern ">") s)
