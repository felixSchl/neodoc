-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.
-- |
-- | ===
-- |
-- | Thoughts:
-- |    * It appears there is never a reason to fail hard. It would be nice if
-- |      we could produce warnings, however -> Write monad?

module Language.Docopt.Solver where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..), isJust, maybe, maybe', isNothing)
import Data.List (List(..), filter, head, foldM, concat, (:), singleton
                , catMaybes, toList, last, init, length)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (foldl)
import Control.MonadPlus (guard)
import Control.Plus (empty)
import Control.Alt ((<|>))
import Data.Monoid (mempty)
import Control.Monad.Error.Class (throwError)
import Data.String (fromCharArray)
import Data.Array as A
import Data.String as Str
import Data.StrMap as StrMap
import Data.StrMap (StrMap())

import Data.String.Ext ((^=))
import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Usage
import Language.Docopt.Parser.Desc (Desc(..))
import Language.Docopt.Value (Value(..))
import Language.Docopt.Option                as O
import Language.Docopt.Parser.Desc           as DE
import Language.Docopt.Parser.Usage          as U
import Language.Docopt.Parser.Usage.Argument as U
import Language.Docopt.Parser.Usage.Option   as UO

data Result = Consumed (List Argument) | Unconsumed (List Argument)

runResult :: Result -> List Argument
runResult (Consumed   xs) = xs
runResult (Unconsumed xs) = xs

solveBranch :: U.Branch                 -- ^ the usage branch
            -> List Desc                -- ^ the option descriptions
            -> Either SolveError Branch -- ^ the canonical usage branch
solveBranch as ds = Branch <$> go as
  where
    go :: U.Branch -> Either SolveError (List Argument)
    go Nil = return Nil
    go (Cons x Nil) = do runResult <$> do solveArgs x Nothing
    go (Cons x (Cons y xs)) = do
      m <- solveArgs x (Just y)
      case m of
        Unconsumed zs -> (zs ++) <$> go (y:xs)
        Consumed   zs -> (zs ++) <$> go xs

    -- | Solve two adjacent arguments.
    -- | Should the first argument be an option with an argument that
    -- | matches an adjacent command or positional, consume the adjacent
    -- | argument from the input (consume).
    solveArgs :: U.Argument
              -> Maybe U.Argument
              -> Either SolveError Result

    solveArgs (U.EOA) _ = Unconsumed <<< singleton <$> return (EOA)
    solveArgs (U.Stdin) _ = Unconsumed <<< singleton <$> return (Stdin)
    solveArgs (U.Command s) _ = Unconsumed <<< singleton <$> return (Command s)
    solveArgs (U.Positional s r) _ = Unconsumed <<< singleton <$> return (Positional s r)

    solveArgs (U.Group o bs r) _
      = Unconsumed <<< singleton <$> do
        flip (Group o) r <$> do
          flip solveBranch ds `traverse` bs

    -- | Resolve the reference by expanding into real
    -- | options, as derived from the descriptions.
    -- |
    -- | XXX: Currently `r` is unused as all descriptions are in a flat list.
    -- |      once option descriptions are keyed, `r` can be used as the lookup
    -- |      into the map of `key => [Description]`
    solveArgs (U.Reference r) _ = do
      return $ Unconsumed (catMaybes $ convert <$> ds)

      where
        -- | Expand `[options]` into optional groups.
        -- | XXX: These groups may have to be required later, once we have a
        -- |      `[required]` tag.
        convert (DE.OptionDesc (DE.Option y)) =
          return
            $ Group
                true
                (singleton $ Branch $ singleton $ Option $
                  (O.Option { flag:       DE.getFlag y.name
                            , name:       DE.getName y.name
                            , arg:        convertArg y.arg
                            , env:        y.env
                            , repeatable: false
                            }))
                false -- XXX: desc options must be
                      --      able to indicate this!
          where
            convertArg (Just (DE.Argument arg)) = return $ O.Argument arg
            convertArg _ = Nothing

        convert _ = Nothing

    solveArgs (U.Option (UO.LOpt o)) y = do

      -- XXX: Is `head` the right thing to do here? What if there are more
      -- matches? That would indicate ambigiutiy and needs to be treated,
      -- possibly with an error?
      let x = O.runOption $ flip maybe' id
                (\_ -> O.Option $ O.empty { name       = pure o.name
                                          , arg        = convertArg o.arg
                                          , repeatable = o.repeatable
                                          })
                (head $ catMaybes $ coerce <$> ds)

      -- Validate the argument matches
      if (argMatches o.arg x.arg)
        then return unit
        else throwError $ DescriptionError $ ArgumentMismatchError {
                option: O.Option x
              , description: {
                  arg: x.arg <#> O.runArgument >>> _.name
                }
              }

      -- Look ahead if any of the following arguments should be consumed.
      -- Return either `Nothing` to signify that nothing should be consumed
      -- or a value signifieng that it should be consumed, and the
      -- `isRepeated` should be inherited.
      let mr = do
            guard (not o.repeatable)
            arg' <- O.runArgument <$> x.arg
            case y of
              Just (U.Positional n r) | n == arg'.name -> return r
              Just (U.Command n)      | n == arg'.name -> return false
              _                                        -> Nothing

      return $ (maybe Unconsumed (const Consumed) mr)
             $ singleton
             $ Option
             $ O.Option (x { repeatable = maybe (x.repeatable) id mr })

      where
        coerce :: Desc -> Maybe O.Option
        coerce (DE.OptionDesc (DE.Option x))
          = coerce' x
            where
              coerce' { name = DE.Long n' }
                | n' ^= o.name
                = return $ O.Option { name:       pure o.name
                                    , flag:       Nothing
                                    , arg:        resolveOptArg o.arg x.arg
                                    , env:        x.env
                                    , repeatable: o.repeatable
                                    }
              coerce' { name = DE.Full f' n' }
                | n' ^= o.name
                = return $ O.Option { name:       pure o.name
                                    , flag:       pure f'
                                    , arg:        resolveOptArg o.arg x.arg
                                    , env:        x.env
                                    , repeatable: o.repeatable
                                    }
              coerce' _ = Nothing
        coerce _ = Nothing

    solveArgs (U.OptionStack (UO.SOpt o)) d = do

      fromSubsumption <|> fromAdjacentArg

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
        -- |    |`- the flag matches - case SENSITIVE
        -- |    `-- the arg  matches - case INSENSITIVE
        -- |
        -- | Should this check, yield a match, slice the matched
        -- | string off the stack and return the remainder option
        -- | stack (if any).
        -- |
        -- | The remaining options, must - in turn - be solved, too.
        -- | However, this solve is simpler as the option is proven
        -- | not to accept an argument at all.

        fromSubsumption = do
          Left SolveFoo

        -- | Last flag method:
        -- |
        -- | Find the last char in the option stack, look it up
        -- | in the descriptions and when it was found to have an
        -- | argument, that matches the adjacent argument, consume
        -- | that argument and produce a list of remaining short
        -- | options.
        -- |
        -- | "armmsg ARG"
        -- |       ~ ~~~
        -- |       ^  ^
        -- |       |  `-- only consumed if description found and
        -- |       |      has matching argument.
        -- |       `----- look for a description of `-g``
        -- |
        -- | The remaining options, must - in turn - be solved, too.
        -- | However, this solve is simpler as the option is proven
        -- | not to accept an argument at all.

        fromAdjacentArg = do
          -- (flag, ...flags) -> (...flags, flag)
          (Tuple fs f) <- do
            return case A.last (o.stack) of
              Just f  -> Tuple (o.flag A.: (fromJust $ A.init o.stack)) f
              Nothing -> Tuple [] o.flag

          -- look the trailing option up in the descriptions
          -- and combine it into the most complete option we
          -- can know about at this point.
          c  <- O.runOption <$> matchDesc true f
          cs <- matchDesc false `traverse` fs

          -- Look ahead if any of the following arguments should be consumed.
          -- Return either `Nothing` to signify that nothing should be consumed
          -- or a value signifieng that it should be consumed, and the
          -- `isRepeated` should be inherited.
          let mr = do
                guard $ not c.repeatable
                arg' <- O.runArgument <$> c.arg
                case d of
                    Just (U.Positional n r) | n == arg'.name -> return r
                    Just (U.Command n)      | n == arg'.name -> return false
                    _                                        -> Nothing

          return $ (maybe Unconsumed (const Consumed) mr)
                 $ (Option <$>
                       toList cs
                    ++ (singleton $ O.Option $ c {
                          repeatable = maybe c.repeatable id mr
                        })
                  )

        matchDesc :: Boolean -> Char -> Either SolveError O.Option
        matchDesc isTrailing f = return $
          maybe' (\_ -> O.Option
                          $ O.empty {
                              flag = pure f
                            , arg = if isTrailing
                                        then convertArg o.arg
                                        else Nothing
                            , repeatable = o.repeatable
                            }
                )
                id
                (head $ catMaybes $ coerce f isTrailing <$> ds)

        coerce :: Char -> Boolean -> Desc -> Maybe O.Option
        coerce f isTrailing (DE.OptionDesc (DE.Option d))
          = coerce' d
            where
              coerce' { name = DE.Flag f' }
                | (f == f') && (isTrailing || isNothing d.arg)
                = do
                    let arg = if isTrailing
                                then resolveOptArg o.arg d.arg
                                else Nothing

                    -- XXX: GUARD NAME HERE
                    if isTrailing && isJust arg

                    return $ O.Option { flag:       pure f
                                      , name:       Nothing
                                      , arg:        arg
                                      , env:        d.env
                                      , repeatable: o.repeatable
                                      }
              coerce' { name = DE.Full f' n' }
                | (f == f') && (isTrailing || isNothing d.arg)
                = do
                    -- XXX: REPEAT FROM ABOVE
                    return $ O.Option { flag:       pure f'
                                      , name:       pure n'
                                      , arg:        arg
                                      , env:        d.env
                                      , repeatable: o.repeatable
                                      }
              coerce' _ = Nothing
        coerce _ _ _ = Nothing

    -- | Resolve an option's argument name against that given in the
    -- | description, returning the most complete argument known.
    resolveOptArg :: Maybe String
                  -> Maybe DE.Argument
                  -> Maybe O.Argument

    resolveOptArg (Just n) Nothing = do
      return $ O.Argument { name: n
                          , default: Nothing }
    resolveOptArg Nothing (Just (DE.Argument a))
      = return $ O.Argument { name: a.name
                            , default: a.default
                            }
    -- XXX: Do we need to guard that `an == a.name` here?
    resolveOptArg (Just an) (Just (DE.Argument a))
      = return $ O.Argument { name: a.name
                            , default: a.default
                            }
    resolveOptArg _ _ = Nothing

    convertArg :: Maybe String -> Maybe O.Argument
    convertArg a = do
      an <- a
      return $ O.Argument { name: an
                          , default: Nothing }

    argMatches :: Maybe String
               -> Maybe O.Argument
               -> Boolean
    argMatches a a'
      =  (isNothing a)
      || (isNothing a && isNothing a')
      || (maybe false id do
          an <- a
          (O.Argument { name: an' }) <- a'
          return (an ^= an')
        )

solveUsage :: U.Usage -> List Desc -> Either SolveError Usage
solveUsage (U.Usage _ bs) ds = Usage <$> do traverse (flip solveBranch ds) bs

solve :: (List U.Usage)
      -> (List Desc)
      -> Either SolveError (List Usage)
solve us ds = traverse (flip solveUsage ds) us
