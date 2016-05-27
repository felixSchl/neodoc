-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.

module Language.Docopt.Solver where

import Prelude
import Data.Array as A
import Data.String as S
import Data.String.Unsafe as US
import Language.Docopt.Argument as Arg
import Language.Docopt.Argument.Option as O
import Language.Docopt.Parser.Desc as DE
import Language.Docopt.Parser.Usage.Option as UO
import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Control.MonadPlus.Partial (mpartition)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Function (on)
import Data.Functor ((<$))
import Data.List (findIndex, groupBy, List(..), length, filter, singleton, toList, catMaybes, head, (:), concat)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just), isNothing, maybe, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.String (fromChar, fromCharArray, toCharArray, toUpper)
import Data.String.Ext ((^=), endsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Language.Docopt.Argument (runBranch, Argument(..), Branch(..), OptionObj)
import Language.Docopt.Errors (SolveError(..))
import Language.Docopt.Parser.Desc (Desc)
import Language.Docopt.Parser.Usage (Usage(..)) as U
import Language.Docopt.Parser.Usage.Argument (Branch, Argument(..)) as U
import Language.Docopt.Usage (Usage(..))

foreign import undefined :: forall a. a

type Reference = String -- [options] reference

data Slurp a
  = Slurp (List a) -- slurp the adjacent argument
  | Keep  (List a) -- keep  the adjacent argument

data ResolveTo a b
  = Resolved   a
  | Unresolved b

instance showResolveTo :: (Show a, Show b) => Show (ResolveTo a b) where
  show (Resolved   a) = "Resolved "   ++ show a
  show (Unresolved b) = "Unresolved " ++ show b

fail :: forall a. String -> Either SolveError a
fail = Left <<< SolveError

solveBranch :: U.Branch                 -- ^ the usage branch
            -> List Desc                -- ^ the option descriptions
            -> Either SolveError Branch -- ^ the canonical usage branch
solveBranch as ds = Branch <$> go as
  where
    go :: U.Branch -> Either SolveError (List Argument)
    go us = do
      concat <<< expand <<< partition <<< groupFree <$> solveArgs us
      where
        groupFree = groupBy (eq `on` isFree)
        partition xs = f <$> (mpartition isResolved <$> xs)
          where f = bimap (_ <#> \a -> case a of
                            (Resolved x) -> x
                            otherwise    -> undefined
                          )
                          (_ <#> \a -> case a of
                            (Unresolved x) -> x
                            otherwise      -> undefined
                          )

        expand = (expand' <$> _)
          where
            -- Resolve references for [options] (references)
            expand' (Tuple args' refs) | length refs > 0 = map args' ds
              where
                map args (Cons (DE.OptionDesc opt) xs) =
                  -- Assuming description and usage have already been merged,
                  -- find all options that are not present in the surrounding
                  -- "free" area and add them.
                  if isJust (findIndex isMatch args)
                     then map args xs
                     else
                      let converted = Group
                            true
                            (singleton $ Branch $ singleton $ Option $
                                { flag:       DE.getFlag opt.name
                                , name:       DE.getName opt.name
                                , arg:        convArg opt.arg
                                , env:        opt.env
                                , repeatable: false
                                }
                            )
                            opt.repeatable
                       in map (converted:args) xs

                  where
                    isMatch (Option o)
                      = if isJust (DE.getFlag opt.name) &&
                           isJust (o.flag)
                           then fromMaybe false do
                                  eq <$> (DE.getFlag opt.name)
                                     <*> o.flag
                            else if isJust (DE.getName opt.name) &&
                                    isJust (o.name)
                                    then fromMaybe false do
                                          (^=) <$> (DE.getName opt.name)
                                               <*> o.name
                                    else false
                    isMatch (Group _ bs _)
                      = any isMatch (concat $ runBranch <$> bs)
                    isMatch _ = false

                    convArg arg = O.Argument <<< DE.runArgument <$> arg
                map args (Cons _ xs) = map args xs
                map args _ = args
            expand' (Tuple args _) = args

        isFree (Resolved a)   = Arg.isFree a
        isFree (Unresolved _) = true

        isResolved (Resolved _) = true
        isResolved _            = false

    solveArgs :: U.Branch -> Either SolveError
                                    (List (ResolveTo Argument
                                                     Reference))
    solveArgs Nil = return Nil
    solveArgs (Cons x Nil) = do
      m <- solveArg x Nothing
      return case m of
           Resolved (Slurp xs) -> Resolved <$> xs
           Resolved (Keep xs)  -> Resolved <$> xs
           Unresolved a        -> singleton $ Unresolved a
    solveArgs (Cons x (Cons y xs)) = do
      m <- solveArg x (Just y)
      case m of
           Resolved (Keep  zs) -> ((Resolved <$> zs) ++ _) <$> solveArgs (y:xs)
           Resolved (Slurp zs) -> ((Resolved <$> zs) ++ _) <$> solveArgs xs
           Unresolved a        -> ((singleton $ Unresolved a) ++ _)
                                    <$> solveArgs (y:xs)

    simpleResolve v = Right $ Resolved $ Keep $ singleton v

    -- | Solve two adjacent arguments.
    -- | Should the first argument be an option with an argument that
    -- | matches an adjacent command or positional, consume the adjacent
    -- | argument from the input (consume).
    solveArg :: U.Argument
              -> Maybe U.Argument
              -> Either SolveError (ResolveTo (Slurp Argument) Reference)

    solveArg (U.EOA) _ = simpleResolve EOA
    solveArg (U.Stdin) _ = simpleResolve Stdin
    solveArg (U.Command s r) _ = simpleResolve $ Command s r
    solveArg (U.Positional s r) _ = simpleResolve $ Positional s r

    solveArg (U.Group o bs r) _
      = Resolved <<< Keep <<< singleton <$> do
        flip (Group o) r <$> do
          flip solveBranch ds `traverse` bs

    solveArg (U.Reference r) _ = do
      return $ Unresolved r

    solveArg (lopt@(U.Option (opt@(UO.LOpt o)))) adjArg = do

      -- Find a matching option description, if any.
      match <- matchDesc o.name

      -- Check to see if this option has an explicitly bound argument.
      -- In this case, a check to consume an adjacent arg must not take place.
      case o.arg of
        (Just exarg) -> do
          -- Note: There is no check to see if an explicit argument is
          --       the same as specified in the option descriptions for
          --       convenience to the user.
          return  $ Resolved
                  $ Keep
                  $ singleton
                  $ Option match

        Nothing -> do
          -- Look ahead if any of the following arguments should be slurped.
          -- Return either `Nothing` to signify that nothing should be slurped
          -- or a value signifieng that it should be slurped, and the
          -- `isRepeated` flag should be inherited.
          let mr = do
                guard (not o.repeatable)
                matchedArg <- O.runArgument <$> match.arg

                case adjArg of
                  Just (U.Positional n r) ->
                    return $ r <$ guardArgs n matchedArg.name
                  Just (U.Command n r) ->
                    return $ r <$ guardArgs n matchedArg.name
                  otherwise ->
                    if not (matchedArg.optional)
                      then
                        return $ fail
                          $ "Option-Argument specified in options-section missing"
                            ++ " --" ++ o.name
                      else Nothing

          case mr of
            Nothing -> return $ Resolved
                              $ Keep
                              $ singleton
                              $ Option match
            Just er -> do
              r <- er
              return  $ Resolved
                      $ Slurp
                      $ singleton
                      $ Option $ match { repeatable = r }

      where
        guardArgs :: String -> String -> Either SolveError Boolean
        guardArgs n n' | n ^= n' = return true
        guardArgs n n' = fail
          $ "Arguments mismatch for option --" ++ o.name ++ ": "
              ++ show n ++ " and " ++ show n'

        matchDesc :: String -> Either SolveError OptionObj
        matchDesc n =
          case filter isMatch ds of
            xs | length xs > 1 -> fail
              $ "Multiple option descriptions for option --" ++ n
            (Cons (DE.OptionDesc desc) Nil) -> do
              arg <- resolveOptArg o.arg desc.arg
              return $ { flag:       DE.getFlag desc.name
                       , name:       DE.getName desc.name
                       , arg:        arg
                       , env:        desc.env
                       , repeatable: o.repeatable
                       }
            -- default fallback: construct the option from itself alone
            _ -> return
                    $ { flag:       Nothing
                      , name:       pure n
                      , env:        Nothing
                      , arg:        convertArg o.arg
                      , repeatable: o.repeatable
                      }
          where
            isMatch (DE.OptionDesc { name = DE.Long n'   }) = n == n'
            isMatch (DE.OptionDesc { name = DE.Full _ n' }) = n == n'
            isMatch _ = false


    solveArg (U.OptionStack (opt@(UO.SOpt o))) adj
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
        -- | string off the stack and return the remainder option
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
                     ++ "may not be subsumed."
            else pure unit

          let fs  = fromCharArray $ o.flag A.: o.stack

          maybe (fail "No description subsumed option")
                return
                -- XXX: Purescript is not lazy, so this is too expensive.
                --      We can just stop at the first `Just` value.
                (head $ catMaybes $ subsume fs <$> ds)

          where
            subsume :: String -> Desc -> Maybe (ResolveTo (Slurp Argument)
                                                          Reference)
            subsume fs (DE.OptionDesc d) = do
              f <- DE.getFlag d.name
              a <- DE.runArgument <$> d.arg

              -- the haystack needs to be modified, such that the
              -- the last (length a.name) characters are uppercased
              -- and hence compared case INSENSITIVELY.
              let needle = toUpper $ fromChar f ++ a.name
                  haystack = toUpper fs

              (Tuple fs o) <- if endsWith needle haystack
                then
                  let ix = S.length haystack - S.length needle
                   in if US.charAt ix fs == f
                        then return
                          $ Tuple (toCharArray (S.take (S.length fs - S.length a.name - 1) fs))
                                  { flag:       pure f
                                  , name:       DE.getName d.name
                                  , arg:        pure $ O.Argument a
                                  , env:        d.env
                                  , repeatable: o.repeatable
                                  }
                    else Nothing
                else Nothing

              cs <- either (const Nothing)
                           (pure <<< id)
                           (matchDesc false `traverse` fs)

              return  $ Resolved
                      $ Keep
                      $ (Option <$> toList cs) ++ (singleton $ Option o)


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
            return case A.last (o.stack) of
              Just f'  -> Tuple (o.flag A.: (fromJust $ A.init o.stack)) f'
              Nothing -> Tuple [] o.flag

          -- Look the trailing option up in the descriptions
          -- and combine it into the most complete option we
          -- can know about at this point.
          match   <- matchDesc true f
          matches <- (Option <$> _) <$> (matchDesc false `traverse` fs)

          -- Check to see if this option has an explicitly bound argument.
          -- In this case, a check to consume an adjacent arg must not take
          -- place.
          case o.arg of
            (Just exarg) -> do
              -- Note: There is no check to see if an explicit argument is
              --       the same as specified in the option descriptions for
              --       convenience to the user.
              return $ Resolved $ Keep
                    $ (toList matches)
                      ++ (singleton $ Option match)

            Nothing ->
              -- Look ahead if any of the following arguments should be slurped.
              -- Return either `Nothing` to signify that nothing should be
              -- slurped or a value signifieng that it should be slurped, and
              -- the `isRepeated` flag should be inherited.
              let mr = do
                    guard $ not match.repeatable
                    arg' <- O.runArgument <$> match.arg
                    case adj of
                      Just (U.Positional n r) ->
                        return $ r <$ guardArgs n arg'.name
                      Just (U.Command n r) ->
                        return $ r <$ guardArgs n arg'.name
                      otherwise ->
                        if not (arg'.optional)
                          then
                            return $ fail
                              $ "Option-Argument specified in options-section missing"
                                ++ " -" ++ fromChar o.flag
                          else Nothing

               in case mr of
                Nothing -> do
                  return $ Resolved $ Keep
                    $ (toList matches)
                      ++ (singleton $ Option match)
                Just er -> do
                  r <- er
                  return $ Resolved $ Slurp
                    $ (toList matches)
                      ++ (singleton
                            $ Option $ match { repeatable = r })

        guardArgs :: String -> String -> Either SolveError Boolean
        guardArgs n n' | n ^= n' = return true
        guardArgs n n' = fail
          $ "Arguments mismatch for option -" ++ fromChar o.flag ++ ": "
              ++ show n ++ " and " ++ show n'


        -- | Match a given flag with an option description.
        -- | `isTrailing` indicates if this flag is the last flag
        -- | in it's stack of flags.

        matchDesc :: Boolean -> Char -> Either SolveError OptionObj
        matchDesc isTrailing f =
          case filter isMatch ds of
            xs | length xs > 1 -> fail
                    $ "Multiple option descriptions for option -"
                        ++ fromChar f

            (Cons (DE.OptionDesc desc) Nil) -> do
              arg <- if isTrailing
                          then resolveOptArg o.arg desc.arg
                          else if isNothing desc.arg
                            then return Nothing
                            else fail
                              $ "Stacked option -" ++ fromChar f
                                  ++ " may not specify arguments"

              return $ { flag:       DE.getFlag desc.name
                       , name:       DE.getName desc.name
                       , arg:        arg
                       , env:        desc.env
                       , repeatable: o.repeatable
                       }

            -- default fallback: construct the option from itself alone
            _ -> return
                    $ { flag: pure f
                      , name: Nothing
                      , env:  Nothing
                      , arg:  if isTrailing
                                   then convertArg o.arg
                                   else Nothing
                      , repeatable: o.repeatable
                      }

          where
            isMatch (DE.OptionDesc { name = DE.Flag f'   }) = f == f'
            isMatch (DE.OptionDesc { name = DE.Full f' _ }) = f == f'
            isMatch _ = false

    -- | Resolve an option's argument name against that given in the
    -- | description, returning the most complete argument known.
    resolveOptArg :: Maybe { name :: String, optional :: Boolean }
                  -> Maybe DE.Argument
                  -> Either SolveError (Maybe O.Argument)

    resolveOptArg (Just a) Nothing = do
      return <<< pure $ O.Argument { name: a.name
                                   , optional: a.optional
                                   , default: Nothing }

    resolveOptArg Nothing (Just (DE.Argument de)) = do
      return <<< pure $ O.Argument { name:     de.name
                                   , optional: de.optional
                                   , default:  de.default }

    resolveOptArg (Just a) (Just (DE.Argument de)) = do
      return <<< pure
        $ O.Argument  { name:     de.name
                      , optional: de.optional || a.optional
                      , default:  de.default
                      }

    resolveOptArg _ _ = return Nothing

    convertArg :: Maybe { name :: String, optional :: Boolean }
               -> Maybe O.Argument
    convertArg arg = do
      a <- arg
      return $ O.Argument { name:     a.name
                          , optional: a.optional
                          , default:  Nothing }

solveUsage :: U.Usage -> List Desc -> Either SolveError Usage
solveUsage (U.Usage _ bs) ds = Usage <$> do traverse (flip solveBranch ds) bs

solve :: List U.Usage
      -> List Desc
      -> Either SolveError (List Usage)
solve us ds = traverse (flip solveUsage ds) us
