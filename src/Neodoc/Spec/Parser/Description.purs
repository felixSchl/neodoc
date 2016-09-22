module Neodoc.Spec.Parser.Description where

import Prelude
import Data.Tuple.Nested ((/\))
import Data.Tuple (swap) as Tuple
import Data.NonEmpty ((:|))
import Data.Functor (($>))
import Data.Function (on)
import Data.Foldable (intercalate, foldl, elem)
import Data.String as Str
import Control.Lazy (defer)
import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.MonadPlus (guard)
import Data.String (singleton) as String
import Data.List (
  List(..), (:), many, some, head, length, filter, catMaybes, reverse,
  singleton)
import Text.Parsing.Parser (ParseError, fail) as P
import Text.Parsing.Parser.Combinators (
  (<?>), try, choice, lookAhead, manyTill,
  option, optionMaybe, optional, notFollowedBy,
  sepBy, sepBy1, (<??>)) as P
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
import Neodoc.Spec.Parser.Combinators
import Neodoc.OptionAlias as OptionAlias
import Neodoc.OptionAlias (Aliases)
import Neodoc.Data.Description

data Content
  = Default String
  | Env     String

parse :: (List L.PositionedToken) -> Either P.ParseError (List Description)
parse = flip L.runTokenParser do
  markIndent do
    reverse <$> go Nil
    <* L.eof

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

  anyName :: L.TokenParser String
  anyName = L.angleName <|> L.shoutName <|> L.name

  desc :: L.TokenParser Description
  desc = defer \_-> "--option or <positional> description" P.<??> do
          P.choice $  [ optionDesc
                      , positionalsDesc
                      ]

  descContent :: Boolean -> L.TokenParser (List Content)
  descContent toplevel = do
    markIndent do
      catMaybes <$> (flip P.manyTill descEnd do
        P.choice $ P.try <$> [
          Just <<< Default <$> L.tag "default"
        , Just <<< Env     <$> L.tag "env"
        , L.anyToken $> Nothing
        ])
    <* (void L.eof <|> void (some L.newline))
    where
      descEnd = do
        P.choice [
          L.eof
        , void $ P.lookAhead do
            L.newline
            when (not toplevel) lessIndented
            P.choice [
              void L.sopt
            , void L.lopt
            , void L.angleName
            , void L.shoutName
            ]
        ]

  positionalsDesc :: L.TokenParser Description
  positionalsDesc = do
    L.angleName <|> L.shoutName
    P.option false $ L.tripleDot $> true
    descContent false
    pure CommandDescription

  optionDesc :: L.TokenParser Description
  optionDesc = do
    { aliases, arg, repeatable } <- opt
    description <- descContent false

    let defaults = getDefaultValue <$> filter isDefaultTag description
        envs     = getEnvKey       <$> filter isEnvTag     description
        default  = head defaults >>= id
        env      = head envs     >>= id

    when (length defaults > 1) do
      P.fail $
        "Option " <> (intercalate ", " $ pretty <$> aliases)
                  <> " has multiple defaults!"

    when (length envs > 1) do
      P.fail $
        "Option " <> (intercalate ", " $ pretty <$> aliases)
                  <> " has multiple environment mappings!"

    when (isJust default && isNothing arg) do
      P.fail $
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

    opt :: L.TokenParser _
    opt = do
      let optsP = "option" P.<??> P.choice [ P.try short, long ]
      xs <- optsP `P.sepBy1` do  -- extra options: [, -f]
        P.optional do
          L.comma
            *> many L.newline
            *> indented

      -- check integrity of the option-aliases
      aliases <- foldl (\acc next -> do
        cur <- acc
        if next `elem` cur
          then P.fail $ "Option appears multiple times: " <> pretty next
          else pure $ cur <> singleton next
      ) (pure Nil) (_.alias <$> xs)

      -- check integrity of the option-arguments
      arg <- foldl (\acc next -> do
        cur <- acc
        case cur of
          Nothing  -> pure (Just next)
          Just arg ->
            if arg.name ^/=^ next.name
              then P.fail $
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


    short :: L.TokenParser _
    short = do
      { flag, arg } <- do
        { chars: c :| cs, arg } <- L.sopt
        (guard $ A.length cs == 0) P.<?> "No stacked options"
        pure { flag: c, arg: arg }

      -- Grab the adjacent positional-looking argument
      -- in case the token did not have an explicit
      -- binding via `=`.
      arg <- maybe
              (P.optionMaybe do
                c <- P.optionMaybe $ P.choice [
                      L.lparen  $> (L.rparen  $> false)
                    , L.lsquare $> (L.rsquare $> true)
                    ]
                name <- L.shoutName <|> L.angleName
                optional <- fromMaybe (pure false) c
                pure { name, optional }
              )
              (pure <<< Just)
              arg

      repeatable <- P.option false $ L.tripleDot $> true

      pure {
        alias: OptionAlias.Short flag
      , arg
      , repeatable
      }

    long :: L.TokenParser _
    long = do
      { name, arg } <- L.lopt

      -- Grab the adjacent positional-looking argument
      -- in case the token did not have an explicit
      -- binding via `=`.
      arg' <- maybe
              (P.optionMaybe do
                c <- P.optionMaybe (P.choice [
                      L.lparen  $> (L.rparen  $> false)
                    , L.lsquare $> (L.rsquare $> true)
                    ])
                n <- L.shoutName <|> L.angleName
                optional <- fromMaybe (pure false) c
                pure { name: n, optional: optional }
              )
              (pure <<< Just)
              arg

      repeatable <- P.option false $ L.tripleDot $> true

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
posArgsEq = eq `on` (Str.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^

notPosArgsEq :: String -> String -> Boolean
notPosArgsEq = not <<< posArgsEq
infixl 9 notPosArgsEq as ^/=^

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (Str.stripPrefix "<" s)
  stripSuffix s = fromMaybe s (Str.stripSuffix ">" s)
