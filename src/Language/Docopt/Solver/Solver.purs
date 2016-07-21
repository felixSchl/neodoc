-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.

module Language.Docopt.Solver where

import Prelude
import Debug.Trace
import Data.Array as A
import Data.String as S
import Data.String.Unsafe as US
import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Control.MonadPlus.Partial (mpartition)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (any, elem)
import Data.Function (on)
import Data.Functor ((<$))
import Data.List (findIndex, groupBy, List(..), length, filter, singleton,
                  fromFoldable, catMaybes, head, (:), concat, reverse)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just), isNothing, maybe, isJust,
                  fromJust)
import Data.String (fromCharArray, toCharArray, toUpper)
import Data.String (singleton) as String
import Data.String.Ext ((^=), endsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.String as Str
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NonEmpty

import Language.Docopt.Argument
import Language.Docopt.Argument (isFree) as Arg
import Language.Docopt.OptionAlias (Aliases(), OptionAlias(..))
import Language.Docopt.OptionAlias (OptionAlias(..)) as OptionAlias
import Language.Docopt.SpecParser.Desc as Desc
import Language.Docopt.Argument (Argument(..), Branch, OptionArgument(..))
import Language.Docopt.Errors (SolveError(..))
import Language.Docopt.SpecParser (Desc(), Usage()) as SpecParser
import Language.Docopt.SpecParser.Usage.Argument (Branch, Argument(..)) as U
import Language.Docopt.Usage (Usage)
import Partial.Unsafe (unsafePartial)

foreign import undefined :: forall a. a

type Reference = String -- [options] reference

data Slurp a
  = Slurp (List a) -- slurp the adjacent argument
  | Keep  (List a) -- keep  the adjacent argument

data ResolveTo a b
  = Resolved   a
  | Unresolved b


posArgsEq :: String -> String -> Boolean
posArgsEq = eq `on` (Str.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (Str.stripPrefix "<" s)
  stripSuffix s = fromMaybe s (Str.stripSuffix ">" s)

instance showResolveTo :: (Show a, Show b) => Show (ResolveTo a b) where
  show (Resolved   a) = "Resolved "   <> show a
  show (Unresolved b) = "Unresolved " <> show b

fail :: forall a. String -> Either SolveError a
fail = Left <<< SolveError

solveBranch
  :: U.Branch                 -- ^ the usage branch
  -> List SpecParser.Desc     -- ^ the option descriptions
  -> Either SolveError Branch -- ^ the canonical usage branch
solveBranch as ds = go as
  where
    go :: U.Branch -> Either SolveError (List Argument)
    go args = do
      solved <- solveArgs args
      pure do
        clump <- groupFree solved
        case partition clump of
          Tuple surroundingArgs _ -> do
            x <- clump
            case x of
              Resolved x   -> pure x
              Unresolved _ -> expand ds surroundingArgs

      where
        groupFree
          :: List (ResolveTo Argument Reference)
          -> List (List (ResolveTo Argument Reference))
        groupFree = groupBy (eq `on` isFree)

        partition
          :: forall a b
           . List (ResolveTo a b)
          -> Tuple (List a) (List b)
        partition = f <<< mpartition isResolved
          where f = bimap (\x -> concat $ x <#> \a -> case a of
                            (Resolved x) -> pure x
                            otherwise    -> Nil
                          )
                          (\x -> concat $ x <#> \a -> case a of
                            (Unresolved x) -> pure x
                            otherwise      -> Nil
                          )

        expand
          :: List SpecParser.Desc
          -> List Argument
          -> List Argument
        expand ds surroundingArgs = reverse $ go ds surroundingArgs Nil
          where
          go :: List SpecParser.Desc -> List Argument -> List Argument -> List Argument
          go (Cons (x@(Desc.OptionDesc desc)) xs) surroundingArgs acc = do
            -- Assuming description and usage have already been merged,
            -- find all options that are not present in the surrounding
            -- "free" area and add them.
            let
                -- XXX: `unsafePartial` is required here since this is
                -- supposed to be a pure function. The change in the
                -- representation of aliases to a stronger type highlights
                -- this potential scenario. This needs to be revisited,
                -- probably by using the same approach for descriptions
                -- themselves.
                aliases = unsafePartial $ fromRight $ convertToAlias desc.name
            if isJust (findIndex (isMatch aliases) surroundingArgs)
                then go xs surroundingArgs acc
                else
                  let z = Group {
                          optional: true
                        , branches: (singleton $ singleton $ Option $
                              { aliases:    aliases
                              , arg:        OptionArgument <$> desc.arg
                              , env:        desc.env
                              , repeatable: false
                              }
                          )
                        , repeatable: desc.repeatable
                        }
                  in go xs (z:surroundingArgs) (z:acc)
            where
              isMatch aliases (Option o)  = any (_ `elem` aliases) o.aliases
              isMatch aliases (Group grp) = any (isMatch aliases) (concat grp.branches)
              isMatch _ _                 = false

          go (Cons _ xs) surroundingArgs acc = go xs surroundingArgs acc
          go _ _ acc = acc

        isFree :: forall b . ResolveTo Argument b -> Boolean
        isFree (Resolved a)   = Arg.isFree a
        isFree (Unresolved _) = true

        isResolved :: forall a b . ResolveTo a b -> Boolean
        isResolved (Resolved _) = true
        isResolved _            = false

    solveArgs :: U.Branch -> Either SolveError
                                    (List (ResolveTo Argument
                                                     Reference))
    solveArgs Nil = pure Nil
    solveArgs (Cons x Nil) = do
      m <- solveArg x Nothing
      pure case m of
           Resolved (Slurp xs) -> Resolved <$> xs
           Resolved (Keep xs)  -> Resolved <$> xs
           Unresolved a        -> singleton $ Unresolved a
    solveArgs (Cons x (Cons y xs)) = do
      m <- solveArg x (Just y)
      case m of
           Resolved (Keep  zs) -> ((Resolved <$> zs) <> _) <$> solveArgs (y:xs)
           Resolved (Slurp zs) -> ((Resolved <$> zs) <> _) <$> solveArgs xs
           Unresolved a        -> ((singleton $ Unresolved a) <> _)
                                    <$> solveArgs (y:xs)

    simpleResolve v = Right $ Resolved $ Keep $ singleton v

    -- | Solve two adjacent arguments.
    -- | Should the first argument be an option with an argument that
    -- | matches an adjacent command or positional, consume the adjacent
    -- | argument from the input (consume).
    solveArg :: U.Argument
              -> Maybe U.Argument
              -> Either SolveError (ResolveTo (Slurp Argument) Reference)

    solveArg (U.EOA) _            = simpleResolve EOA
    solveArg (U.Stdin) _          = simpleResolve Stdin
    solveArg (U.Command cmd) _    = simpleResolve $ Command {
                                      name:       cmd.name
                                    , repeatable: cmd.repeatable
                                    }
    solveArg (U.Positional pos) _ = simpleResolve $ Positional {
                                      name:       pos.name
                                    , repeatable: pos.repeatable
                                    }

    solveArg (U.Group grp) _
      = Resolved <<< Keep <<< singleton <$> do
          branches <- flip solveBranch ds `traverse` grp.branches
          pure $ Group grp { branches = branches }

    solveArg (U.Reference r) _ = do
      pure $ Unresolved r

    solveArg (U.Option o) mAdjArg = do

      -- Find a matching option description, if any.
      descMatch <- matchDesc o.name

      -- Check to see if this option has an explicitly bound argument.
      -- In this case, a check to consume an adjacent arg must not take place.
      case o.arg of
        (Just exarg) -> do
          -- Note: There is no check to see if an explicit argument is
          --       the same as specified in the option descriptions for
          --       convenience to the user.
          pure  $ Resolved
                $ Keep
                $ singleton
                $ Option descMatch

        Nothing -> do
          -- Look ahead if any of the following arguments should be slurped.
          -- Return either `Nothing` to signify that nothing should be slurped
          -- or a value signifieng that it should be slurped, and the
          -- `isRepeated` flag should be inherited.
          let
            adjArgMatch = do
              guard (not o.repeatable)
              (arg@{ name }) <- unOptionArgument <$> descMatch.arg
              adjArg <- mAdjArg
              (r /\ n /\ optional) <- case adjArg of
                (U.Positional pos) -> pure (pos.repeatable /\ pos.name /\ false)
                (U.Command    cmd) -> pure (cmd.repeatable /\ cmd.name /\ false)
                (U.Group { branches: (x : Nil) : Nil, optional, repeatable }) ->
                  case x of
                    (U.Positional pos) ->
                      pure ((pos.repeatable || repeatable) /\ pos.name /\ optional)
                    (U.Command    cmd) ->
                      pure ((cmd.repeatable || repeatable) /\ cmd.name /\ optional)
                    otherwise -> Nothing
                otherwise -> Nothing
              pure do
                guardArgs n name
                pure (r /\ arg { optional = optional })

          case adjArgMatch of
            Nothing -> do
              if not (fromMaybe true
                      $ (_.optional <<< unOptionArgument <$> descMatch.arg))
                 then do
                    fail
                      $ "Option-Argument specified in options-section missing"
                        <> " --" <> o.name
                  else
                    pure  $ Resolved
                          $ Keep
                          $ singleton
                          $ Option descMatch
            Just matched -> do
              (r /\ arg) <- matched
              pure  $ Resolved
                    $ Slurp
                    $ singleton
                    $ Option $ descMatch  { arg = pure (OptionArgument arg)
                                          , repeatable = r
                                          }

      where
        guardArgs :: String -> String -> Either SolveError Boolean
        guardArgs n n' | n ^=^ n' = pure true
        guardArgs n n' = fail
          $ "Arguments mismatch for option --" <> o.name <> ": "
              <> show n <> " and " <> show n'

        matchDesc :: String -> Either SolveError OptionObj
        matchDesc n =
          case filter isMatch ds of
            xs | length xs > 1 -> fail
              $ "Multiple option descriptions for option --" <> n
            (Cons (Desc.OptionDesc desc) Nil) -> do
              arg <- resolveOptArg o.arg desc.arg

              -- coerce the descriptions representation of an option name
              -- into that understood by `D.Argument`.
              -- TODO: update the descriptions representation to something
              --       similar.
              aliases <- convertToAlias desc.name

              pure  { aliases:    aliases
                    , arg:        OptionArgument <$> arg
                    , env:        desc.env
                    , repeatable: o.repeatable
                    }
            -- default fallback: construct the option from itself alone
            otherwise ->
              pure  { aliases:    NonEmpty (OptionAlias.Long n) Nil
                    , env:        Nothing
                    , arg:        OptionArgument <$> convertArg o.arg
                    , repeatable: o.repeatable
                    }
          where
            isMatch (Desc.OptionDesc { name: Desc.Long n'   }) = n == n'
            isMatch (Desc.OptionDesc { name: Desc.Full _ n' }) = n == n'
            isMatch _ = false


    solveArg (U.OptionStack o) mAdjArg
      = fromSubsumption <|> fromAdjacentArgOrDefault

      where
        -- | Subsumption method:
        -- | (Only applies for options w/o explicit arg)
        -- |
        -- | Concatenate the option stack into a string,
        -- | then do a substring check for each possible description.
        -- |
        -- | "armmsg"
        -- |    ~~~~
        -- |    ^^
        -- |    |`- the arg  matches - case INSENSITIVE
        -- |    `-- the flag matches - case SENSITIVE
        -- |
        -- | Should this check yield a match, slice the matched
        -- | string off the stack and pure the remainder option
        -- | stack (if any).
        -- |
        -- | The remaining options, must - in turn - be solved, too.
        -- | However, this solve is simpler as the option is proven
        -- | not to accept an argument at all.

        fromSubsumption :: Either SolveError
                                  (ResolveTo (Slurp Argument)
                                              Reference)
        fromSubsumption = do

          -- XXX: Well, this error should not be thrown, but rather,
          --      we should just not come to this code-path if this is
          --      the case. It would be an awkward user-facing error.
          if isJust o.arg
            then fail $ "Option stacks with explicit argument binding "
                     <> "may not be subsumed."
            else pure unit

          let fs  = fromCharArray $ o.flag A.: o.stack

          maybe (fail "No description subsumed option")
                pure
                -- XXX: Purescript is not lazy, so this is too expensive.
                --      We can just stop at the first `Just` value.
                (head $ catMaybes $ subsume fs <$> ds)

          where
            subsume
              :: String
              -> SpecParser.Desc
              -> Maybe (ResolveTo (Slurp Argument)
                       Reference)
            subsume fs (Desc.OptionDesc d) = do
              f <- Desc.getFlag d.name
              a <- d.arg

              -- the haystack needs to be modified, such that the
              -- the last (length a.name) characters are uppercased
              -- and hence compared case INSENSITIVELY.
              let bareArgname = stripAngles a.name
                  needle      = toUpper $ String.singleton f <> bareArgname
                  haystack    = toUpper fs

              (Tuple fs o) <- if endsWith needle haystack
                then
                  let ix = S.length haystack - S.length needle
                   in if unsafePartial (US.charAt ix fs) == f
                        then pure
                          $ Tuple (toCharArray
                                    (S.take (S.length fs
                                              - S.length bareArgname
                                              - 1
                                            ) fs
                                    )
                                  )
                                  { aliases:
                                      OptionAlias.Short f
                                        :| maybe  Nil
                                            (singleton <<< OptionAlias.Long)
                                            (Desc.getName d.name)
                                  , arg:        pure a
                                  , env:        d.env
                                  , repeatable: o.repeatable
                                  }
                    else Nothing
                else Nothing

              cs <- either (const Nothing)
                           (pure <<< id)
                           (matchDesc false `traverse` fs)

              -- set the same repeatability flag for each stacked option as
              -- indicated by trailing option.
              let cs' = flip setRepeatable o.repeatable <$> do
                          Option <$> fromFoldable cs

              pure  $ Resolved
                    $ Keep
                    $ cs' <> (singleton $ Option $ o {
                      arg = OptionArgument <$> o.arg
                    })

            subsume _ _ = Nothing

        -- | Last flag method (w/ fallback):
        -- |
        -- | Find the last char in the option stack, look it up
        -- | in the descriptions and when it was found to have an
        -- | argument, that matches the adjacent argument (or it's
        -- | explicit binding), consume that argument and produce
        -- | a list of remaining short options.
        -- |
        -- | "armmsg=ARG"
        -- | OR:
        -- | "armmsg ARG"
        -- |       ~ ~~~
        -- |       ^  ^
        -- |       |  `-- only slurped if description found and
        -- |       |      has matching argument.
        -- |       `----- look for a description of `-g``
        -- |
        -- | The remaining options, must - in turn - be solved, too.
        -- | However, this solve is simpler as the option is proven
        -- | not to accept an argument at all.

        fromAdjacentArgOrDefault :: Either SolveError
                                            (ResolveTo (Slurp Argument)
                                                        Reference)
        fromAdjacentArgOrDefault = do

          -- (flag, ...flags) -> (...flags, flag)
          (Tuple fs f) <- do
            pure case A.last (o.stack) of
              Just f' -> Tuple (o.flag A.: (unsafePartial
                                            $ fromJust
                                            $ A.init o.stack)) f'
              Nothing -> Tuple [] o.flag

          -- Look the trailing option up in the descriptions
          -- and combine it into the most complete option we
          -- can know about at this point.
          descMatch <- matchDesc true f
          matches   <- (Option <$> _) <$> (matchDesc false `traverse` fs)

          -- Check to see if this option has an explicitly bound argument.
          -- In this case, a check to consume an adjacent arg must not take
          -- place.
          case o.arg of
            (Just exarg) -> do
              -- Note: There is no check to see if an explicit argument is
              --       the same as specified in the option descriptions for
              --       convenience to the user.
              pure $ Resolved $ Keep
                    $ (fromFoldable matches)
                      <> (singleton $ Option descMatch)

            Nothing ->
              -- Look ahead if any of the following arguments should be slurped.
              -- Return either `Nothing` to signify that nothing should be
              -- slurped or a value signifieng that it should be slurped, and
              -- the `isRepeated` flag should be inherited.
              let
                adjArgMatch = do
                  guard (not o.repeatable)
                  (arg@{ name }) <- unOptionArgument <$> descMatch.arg
                  adjArg <- mAdjArg
                  (r /\ n /\ optional) <- case adjArg of
                    (U.Positional pos) -> pure (pos.repeatable /\ pos.name /\ false)
                    (U.Command    cmd) -> pure (cmd.repeatable /\ cmd.name /\ false)
                    (U.Group { branches: (x : Nil) : Nil, optional, repeatable }) ->
                      case x of
                        (U.Positional pos) ->
                          pure ((pos.repeatable || repeatable) /\ pos.name /\ optional)
                        (U.Command    cmd) ->
                          pure ((cmd.repeatable || repeatable) /\ cmd.name /\ optional)
                        otherwise -> Nothing
                    otherwise -> Nothing
                  pure do
                    guardArgs n name
                    pure (r /\ arg { optional = optional })

              in case adjArgMatch of
                Nothing -> do
                  if not (fromMaybe true
                           $ _.optional <<< unOptionArgument <$> descMatch.arg)
                    then fail
                        $ "Option-Argument specified in options-section missing"
                          <> " -" <> String.singleton f
                    else do
                      pure $ Resolved $ Keep
                        $ (fromFoldable matches)
                          <> (singleton $ Option descMatch)
                Just matched -> do
                  (r /\ arg) <- matched
                  pure $ Resolved $ Slurp
                    $ (flip setRepeatable r <$> fromFoldable matches)
                      <> (singleton
                            $ Option $ descMatch  { arg = pure (OptionArgument arg)
                                                  , repeatable = r
                                                  })

        guardArgs :: String -> String -> Either SolveError Boolean
        guardArgs n n' | n ^=^ n' = pure true
        guardArgs n n' = fail
          $ "Arguments mismatch for option -" <> String.singleton o.flag <> ": "
              <> show n <> " and " <> show n'


        -- | Match a given flag with an option description.
        -- | `isTrailing` indicates if this flag is the last flag
        -- | in it's stack of flags.

        matchDesc :: Boolean -> Char -> Either SolveError OptionObj
        matchDesc isTrailing f =
          case filter isMatch ds of
            xs | length xs > 1 -> fail
                    $ "Multiple option descriptions for option -"
                        <> String.singleton f

            (Cons (Desc.OptionDesc desc) Nil) -> do
              arg <- if isTrailing
                          then resolveOptArg o.arg desc.arg
                          else if isNothing desc.arg
                            then pure Nothing
                            else fail
                              $ "Stacked option -" <> String.singleton f
                                  <> " may not specify arguments"
              aliases <- convertToAlias desc.name

              pure  { aliases:    aliases
                    , arg:        OptionArgument <$> arg
                    , env:        desc.env
                    , repeatable: o.repeatable
                    }

            -- default fallback: construct the option from itself alone
            otherwise ->
              pure  { aliases:    (Short f) :| Nil
                    , env:        Nothing
                    , arg:        OptionArgument <$>
                                    if isTrailing
                                      then convertArg o.arg
                                      else Nothing
                    , repeatable: o.repeatable
                    }

          where
            isMatch (Desc.OptionDesc { name: Desc.Flag f'   }) = f == f'
            isMatch (Desc.OptionDesc { name: Desc.Full f' _ }) = f == f'
            isMatch _ = false

    -- | Resolve an option's argument name against that given in the
    -- | description, pureing the most complete argument known.
    resolveOptArg :: Maybe { name :: String, optional :: Boolean }
                  -> Maybe Desc.OptionArgumentObj
                  -> Either SolveError (Maybe OptionArgumentObj)

    resolveOptArg (Just a) Nothing = do
      pure <<< pure $ { name:     a.name
                      , optional: a.optional
                      , default:  Nothing }

    resolveOptArg Nothing (Just de) = do
      pure <<< pure $ { name:     de.name
                      , optional: de.optional
                      , default:  de.default }

    resolveOptArg (Just a) (Just de) = do
      pure <<< pure
        $ { name:     de.name
          , optional: de.optional || a.optional
          , default:  de.default
          }

    resolveOptArg _ _ = pure Nothing

    convertArg :: Maybe { name :: String, optional :: Boolean }
               -> Maybe OptionArgumentObj
    convertArg arg = do
      a <- arg
      pure  { name:     a.name
            , optional: a.optional
            , default:  Nothing }

    convertToAlias
      :: Desc.Name
      -> Either SolveError Aliases
    convertToAlias n =
      maybe (fail "Unnamed option") pure do
                  ((:|) <$> (Short <$> Desc.getFlag n)
                        <*> ((singleton <<< Long <$> Desc.getName n)
                                    <|> pure Nil)
                  ) <|>
                  ((:|) <$> (Long <$> Desc.getName n)
                        <*> ((singleton <<< Short <$> Desc.getFlag n)
                                    <|> pure Nil)
                  )

solveUsage
  :: SpecParser.Usage
  -> List SpecParser.Desc
  -> Either SolveError Usage
solveUsage bs ds = traverse (flip solveBranch ds) bs

solve
  :: List SpecParser.Usage
  -> List SpecParser.Desc
  -> Either SolveError (List Usage)
solve us ds = traverse (flip solveUsage ds) us
