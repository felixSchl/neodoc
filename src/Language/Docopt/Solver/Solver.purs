-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.

module Language.Docopt.Solver where

import Prelude
import Data.Functor ((<$))
import Data.Either (Either(..), either)
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(Nothing, Just), isNothing, maybe, isJust)
import Data.List (List(..), length, filter, singleton, toList, catMaybes, head,
                  (:))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Data.String (fromChar, fromCharArray, toCharArray, toUpper)
import Data.Array as A
import Data.String as S
import Data.String.Unsafe as US

import Data.String.Ext ((^=), endsWith)
import Language.Docopt.Errors (SolveError(..))
import Language.Docopt.Argument (Argument(..), Branch(..))
import Language.Docopt.Usage (Usage(..))
import Language.Docopt.Parser.Desc (Desc())
import Language.Docopt.Option as O
import Language.Docopt.Parser.Desc as DE
import Language.Docopt.Parser.Usage (Usage(..)) as U
import Language.Docopt.Parser.Usage.Argument (Branch, Argument(..)) as U
import Language.Docopt.Parser.Usage.Option as UO

data Slurp a
  = Slurp (List a)
  | Keep  (List a)

data Node
  = Resolved   (Slurp Argument)
  | Unresolved U.Argument

fail :: forall a. String -> Either SolveError a
fail = Left <<< SolveError

solveBranch :: U.Branch                 -- ^ the usage branch
            -> List Desc                -- ^ the option descriptions
            -> Either SolveError Branch -- ^ the canonical usage branch
solveBranch as ds = Branch <$> solveArgs as
  where
    solveArgs :: U.Branch -> Either SolveError (List Argument)
    solveArgs Nil = return Nil
    solveArgs (Cons x Nil) = do
      m <- solveArg x Nothing
      return case m of
           Resolved (Slurp xs) -> xs
           Resolved (Keep xs)  -> xs
           Unresolved (_)      -> Nil -- XXX
    solveArgs (Cons x (Cons y xs)) = do
      m <- solveArg x (Just y)
      case m of
           Resolved (Keep  zs) -> (zs ++ _) <$> solveArgs (y:xs)
           Resolved (Slurp zs) -> (zs ++ _) <$> solveArgs xs
           Unresolved (_)      -> return Nil -- XXX

    simpleResolve v = Right $ Resolved $ Keep $ singleton v

    -- | Solve two adjacent arguments.
    -- | Should the first argument be an option with an argument that
    -- | matches an adjacent command or positional, consume the adjacent
    -- | argument from the input (consume).
    solveArg :: U.Argument
              -> Maybe U.Argument
              -> Either SolveError Node

    solveArg (U.EOA) _ = simpleResolve EOA
    solveArg (U.Stdin) _ = simpleResolve Stdin
    solveArg (U.Command s r) _ = simpleResolve $ Command s r
    solveArg (U.Positional s r) _ = simpleResolve $ Positional s r

    solveArg (U.Group o bs r) _
      = Resolved <<< Keep <<< singleton <$> do
        flip (Group o) r <$> do
          flip solveBranch ds `traverse` bs

    solveArg r@(U.Reference _) _ = do
      return $ Unresolved r

    solveArg (lopt@(U.Option (opt@(UO.LOpt o)))) adjArg = do

      -- Find a matching option description, if any.
      match <- O.runOption <$> matchDesc o.name

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
                  $ Option $ O.Option match

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
                  _ -> return $ fail
                      $ "Option-Argument specified in options-section missing"
                        ++ " --" ++ o.name

          case mr of
            Nothing -> return $ Resolved
                              $ Keep
                              $ singleton
                              $ Option $ O.Option match
            Just er -> do
              r <- er
              return  $ Resolved
                      $ Slurp
                      $ singleton
                      $ Option $ O.Option $ match { repeatable = r }

      where
        guardArgs :: String -> String -> Either SolveError Boolean
        guardArgs n n' | n ^= n' = return true
        guardArgs n n' = fail
          $ "Arguments mismatch for option --" ++ o.name ++ ": "
              ++ show n ++ " and " ++ show n'

        matchDesc :: String -> Either SolveError O.Option
        matchDesc n =
          case filter isMatch ds of
            xs | length xs > 1 -> fail
              $ "Multiple option descriptions for option --" ++ n
            (Cons (DE.OptionDesc (DE.Option desc)) Nil) -> do
              arg <- resolveOptArg o.arg desc.arg
              return $ O.Option { flag:       DE.getFlag desc.name
                                , name:       DE.getName desc.name
                                , arg:        arg
                                , env:        desc.env
                                , repeatable: o.repeatable
                                }
            -- default fallback: construct the option from itself alone
            _ -> return $ O.Option
                    $ { flag:       Nothing
                      , name:       pure n
                      , env:        Nothing
                      , arg:        convertArg o.arg
                      , repeatable: o.repeatable
                      }
          where
            isMatch (DE.OptionDesc (DE.Option { name = DE.Long n'   })) = n == n'
            isMatch (DE.OptionDesc (DE.Option { name = DE.Full _ n' })) = n == n'
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
        -- | Should this check, yield a match, slice the matched
        -- | string off the stack and return the remainder option
        -- | stack (if any).
        -- |
        -- | The remaining options, must - in turn - be solved, too.
        -- | However, this solve is simpler as the option is proven
        -- | not to accept an argument at all.

        fromSubsumption :: Either SolveError Node
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
            subsume :: String -> Desc -> Maybe Node
            subsume fs (DE.OptionDesc (DE.Option d)) = do
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
                                  (O.Option {
                                    flag:       pure f
                                  , name:       DE.getName d.name
                                  , arg:        pure $ O.Argument a
                                  , env:        d.env
                                  , repeatable: o.repeatable
                                  })
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

        fromAdjacentArgOrDefault :: Either SolveError Node
        fromAdjacentArgOrDefault = do

          -- (flag, ...flags) -> (...flags, flag)
          (Tuple fs f) <- do
            return case A.last (o.stack) of
              Just f'  -> Tuple (o.flag A.: (fromJust $ A.init o.stack)) f'
              Nothing -> Tuple [] o.flag

          -- Look the trailing option up in the descriptions
          -- and combine it into the most complete option we
          -- can know about at this point.
          match   <- O.runOption <$> matchDesc true f
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
                      ++ (singleton $ Option $ O.Option match)

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
                      _ -> return $ fail
                          $ "Option-Argument specified in options-section missing"
                            ++ " -" ++ fromChar o.flag

               in case mr of
                Nothing -> do
                  return $ Resolved $ Keep
                    $ (toList matches)
                      ++ (singleton $ Option $ O.Option match)
                Just er -> do
                  r <- er
                  return $ Resolved $ Slurp
                    $ (toList matches)
                      ++ (singleton
                            $ Option $ O.Option match { repeatable = r })

        guardArgs :: String -> String -> Either SolveError Boolean
        guardArgs n n' | n ^= n' = return true
        guardArgs n n' = fail
          $ "Arguments mismatch for option -" ++ fromChar o.flag ++ ": "
              ++ show n ++ " and " ++ show n'


        -- | Match a given flag with an option description.
        -- | `isTrailing` indicates if this flag is the last flag
        -- | in it's stack of flags.

        matchDesc :: Boolean -> Char -> Either SolveError O.Option
        matchDesc isTrailing f =
          case filter isMatch ds of
            xs | length xs > 1 -> fail
                    $ "Multiple option descriptions for option -"
                        ++ fromChar f

            (Cons (DE.OptionDesc (DE.Option desc)) Nil) -> do
              arg <- if isTrailing
                          then resolveOptArg o.arg desc.arg
                          else if isNothing desc.arg
                            then return Nothing
                            else fail
                              $ "Stacked option -" ++ fromChar f
                                  ++ " may not specify arguments"

              return $ O.Option { flag:       DE.getFlag desc.name
                                , name:       DE.getName desc.name
                                , arg:        arg
                                , env:        desc.env
                                , repeatable: o.repeatable
                                }

            -- default fallback: construct the option from itself alone
            _ -> return $ O.Option
                    $ { flag: pure f
                      , name: Nothing
                      , env:  Nothing
                      , arg:  if isTrailing
                                   then convertArg o.arg
                                   else Nothing
                      , repeatable: o.repeatable
                      }

          where
            isMatch (DE.OptionDesc (DE.Option { name = DE.Flag f'   })) = f == f'
            isMatch (DE.OptionDesc (DE.Option { name = DE.Full f' _ })) = f == f'
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
