module Language.Docopt.SpecParser.Desc (
    Desc (..)
  , OptionObj ()
  , OptionArgumentObj ()
  , prettyPrintDesc
  , run
  , parse
  ) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple (Tuple))
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
import Data.List (List(..), (:), many, some, head, length, filter, catMaybes,
                  reverse, singleton)
import Text.Parsing.Parser (ParseError, fail) as P
import Text.Parsing.Parser.Combinators ((<?>), try, choice, lookAhead, manyTill,
                                        option, optionMaybe, optional, notFollowedBy,
                                        sepBy, sepBy1, (<??>)) as P
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Nothing, Just), isJust, isNothing, maybe, fromMaybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Array as A
import Data.String.Ext ((^=))

import Language.Docopt.Value (Value, prettyPrintValue)
import Language.Docopt.SpecParser.Common (sameIndent, markIndent, indented,
                                     moreIndented, lessIndented)
import Language.Docopt.SpecParser.Lexer (lexDescs)
import Language.Docopt.SpecParser.Lexer as L
import Language.Docopt.OptionAlias (Aliases(), OptionAlias(), (:|), prettyPrintOptionAlias)
import Language.Docopt.OptionAlias (OptionAlias(..)) as OptionAlias
import Language.Docopt.Value as Value
import Partial.Unsafe

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

data Desc
  = OptionDesc OptionObj
  | CommandDesc

type OptionObj = {
  aliases    :: Aliases
, arg        :: Maybe OptionArgumentObj
, env        :: Maybe String
, repeatable :: Boolean
}

showOptionObj :: OptionObj -> String
showOptionObj o = "{ aliases: "    <> show o.aliases
               <> ", arg: "        <> show (OptionArgument <$> o.arg)
               <> ", env: "        <> show o.env
               <> ", repeatable: " <> show o.repeatable
               <> "}"

eqOptionObj :: OptionObj -> OptionObj -> Boolean
eqOptionObj o o' = o.aliases                  == o'.aliases
                && (OptionArgument <$> o.arg) == (OptionArgument <$> o'.arg)
                && o.env                      == o'.env
                && o.repeatable               == o'.repeatable

type OptionArgumentObj = {
  name     :: String
, default  :: Maybe Value
, optional :: Boolean
}

showOptionArgumentObj :: OptionArgumentObj -> String
showOptionArgumentObj o = "{ name: "     <> show o.name
                       <> ", default: "  <> show o.default
                       <> ", optional: " <> show o.optional
                       <> "}"

eqOptionArgumentObj :: OptionArgumentObj -> OptionArgumentObj -> Boolean
eqOptionArgumentObj a a' = a.name     == a'.name
                        && a.default  == a'.default
                        && a.optional == a'.optional

newtype OptionArgument = OptionArgument OptionArgumentObj

unOptionArgument :: OptionArgument -> OptionArgumentObj
unOptionArgument (OptionArgument a) = a

instance showOptionArgument :: Show OptionArgument where
  show = showOptionArgumentObj <<< unOptionArgument

instance eqOptionArgument :: Eq OptionArgument where
  eq = eqOptionArgumentObj `on` unOptionArgument

data Content
  = Default String
  | Env     String

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

derive instance genericContent :: Generic Content

prettyPrintDesc :: Desc -> String
prettyPrintDesc (OptionDesc opt) = "Option " <> prettyPrintOption opt
prettyPrintDesc (CommandDesc) = "Command"

instance showDesc :: Show Desc where
  show (OptionDesc o) = "OptionDesc " <> showOptionObj o
  show (CommandDesc)  = "CommandDesc"

instance eqDesc :: Eq Desc where
  eq (OptionDesc o) (OptionDesc o') = eqOptionObj o o'
  eq (CommandDesc) (CommandDesc)    = true
  eq _             _                = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption opt
  = aliases <> arg <> env
  where
      aliases = intercalate ", " (prettyPrintOptionAlias <$> opt.aliases)

      arg = maybe "" id do
        a <- opt.arg
        pure $
          (if a.optional then "[" else "")
            <> "=" <> a.name
            <> (if a.optional then "]" else "")
            <> (if opt.repeatable then "..." else "")
            <> (maybe ""
                      (\v -> " [default: " <> prettyPrintValue v <> "]")
                      a.default)

      env = maybe "" id do
        k <- opt.env
        pure $ " [env: " <> k <> "]"

prettyPrintOptionArgument :: _ -> String
prettyPrintOptionArgument { optional: o, name: n }
  = (if o then "[" else "") <> n <> (if o then "]" else "")

run :: String -> Either P.ParseError (List Desc)
run = lexDescs >=> parse

parse :: (List L.PositionedToken) -> Either P.ParseError (List Desc)
parse = flip L.runTokenParser descParser

descParser :: L.TokenParser (List Desc)
descParser = markIndent do
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

    desc :: L.TokenParser Desc
    desc = defer \_-> "--option or <positional> description" P.<??> do
            P.choice $ [ optionDesc
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
              when (not toplevel)
                lessIndented
              P.choice [
                void L.sopt
              , void L.lopt
              , void L.angleName
              , void L.shoutName
              ]
          ]

    positionalsDesc :: L.TokenParser Desc
    positionalsDesc = do
      L.angleName <|> L.shoutName
      P.option false $ L.tripleDot $> true
      descContent false
      pure CommandDesc

    optionDesc :: L.TokenParser Desc
    optionDesc = do

      xopt        <- opt
      description <- descContent false

      let defaults = getDefaultValue <$> filter isDefaultTag description
          envs     = getEnvKey       <$> filter isEnvTag     description
          default  = head defaults >>= id
          env      = head envs     >>= id

      when (length defaults > 1) do
        P.fail $
          "Option " <> (intercalate ", " $ prettyPrintOptionAlias <$> xopt.aliases)
                    <> " has multiple defaults!"

      when (length envs > 1) do
        P.fail $
          "Option " <> (intercalate ", " $ prettyPrintOptionAlias <$> xopt.aliases)
                    <> " has multiple environment mappings!"

      when (isJust default && isNothing xopt.arg) do
        P.fail $
          "Option " <> (intercalate ", " $ prettyPrintOptionAlias <$> xopt.aliases)
                    <> " does not take arguments. "
                    <> "Cannot specify defaults."

      pure $ OptionDesc $
        xopt  { env = env
              , arg = do
                  { name, optional } <- xopt.arg
                  pure $ { name, optional, default }
              }

      where

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
                    n <- L.shoutName <|> L.angleName
                    optional <- fromMaybe (pure false) c
                    pure { name: n, optional: optional }
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
              then P.fail $
                "Option appears multiple times: "
                  <> prettyPrintOptionAlias next
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
                      <> (show $ prettyPrintOptionArgument arg)
                      <> " and "
                      <> (show $ prettyPrintOptionArgument next)
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
