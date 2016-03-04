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
import qualified Data.Array as A
import qualified Data.String as Str

import Language.Docopt.Errors
import Language.Docopt.Argument
import Language.Docopt.Usage
import Language.Docopt.Parser.Desc (Desc(..))
import Language.Docopt.Value (Value(..))
import qualified Language.Docopt.Option                as O
import qualified Language.Docopt.Parser.Desc           as Desc
import qualified Language.Docopt.Parser.Usage          as U
import qualified Language.Docopt.Parser.Usage.Argument as U
import qualified Language.Docopt.Parser.Usage.Option   as UO

data Result = Consumed (List Argument) | Unconsumed (List Argument)

(^=) :: String -> String -> Boolean
(^=) a b = Str.toUpper a == Str.toUpper b

solveBranch :: U.Branch -> List Desc -> Either SolveError Branch
solveBranch as ds = Branch <$> f as
  where f :: U.Branch -> Either SolveError (List Argument)
        f Nil = return Nil
        f (Cons x Nil) = do
          m <- solveArgs x Nothing
          return $ case m of
            Unconsumed zs -> zs
            Consumed   zs -> zs
        f (Cons x (Cons y xs)) = do
          m <- solveArgs x (Just y)
          case m of
            Unconsumed zs -> (zs ++) <$> f (y:xs)
            Consumed   zs -> (zs ++) <$> f xs

        -- | Solve two adjacent arguments.
        -- | Should the first argument be an option with an argument that
        -- | matches an adjacent command or positional, consume the adjacent
        -- | argument from the input (consume).
        solveArgs :: U.Argument
                  -> Maybe U.Argument
                  -> Either SolveError Result

        solveArgs (U.EOA) _
          = Unconsumed <<< singleton <$> return (EOA)

        solveArgs (U.Stdin) _
          = Unconsumed <<< singleton <$> return (Stdin)

        solveArgs (U.Command s) _
          = Unconsumed <<< singleton <$> return (Command s)

        solveArgs (U.Positional s r) _
          = Unconsumed <<< singleton <$> return (Positional s r)

        solveArgs (U.Group o bs r) _
          = Unconsumed <<< singleton <$> do
            flip (Group o) r <$> do
              flip solveBranch ds `traverse` bs

        solveArgs (U.Option (UO.LOpt o)) y = do

          -- XXX: Is `head` the right thing to do here? What if there are more
          -- matches? That would indicate ambigiutiy and needs to be treated,
          -- possibly with an error?
          let x = O.runOption $ flip maybe' id
                    (\_ -> O.lopt' o.name (toArg o.arg) (o.repeatable))
                    (head $ catMaybes $ convert <$> ds)

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
            convert :: Desc -> Maybe O.Option
            convert (Desc.OptionDesc (Desc.Option x))
              = convert' x
                where
                  convert' { name = Desc.Long n', arg = arg' }
                    | n' ^= o.name
                    = return $ O.lopt' (o.name)
                                       (resolveOptArg o.arg arg')
                                       (o.repeatable)
                  convert' { name = Desc.Full f' n', arg = arg' }
                    | n' ^= o.name
                    = return $ O.opt' (Just f')
                                      (Just o.name)
                                      (resolveOptArg o.arg arg')
                                      (o.repeatable)
            convert _ = Nothing

        solveArgs (U.OptionStack (UO.SOpt o)) y = do

          -- Figure out trailing flag, in order to couple it with an adjacent
          -- option where needed.
          (Tuple xs x) <- do
              let fs = toList o.stack
                  t  = case last fs of
                        Just f  -> Tuple (o.flag:(fromJust $ init fs)) f
                        Nothing -> Tuple Nil o.flag
              Tuple
                <$> (match false `traverse` (fst t))
                <*> (O.runOption <$> do match true (snd t))

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
                guard (not x.repeatable)
                arg' <- O.runArgument <$> x.arg
                case y of
                    Just (U.Positional n r) | n == arg'.name -> return r
                    Just (U.Command n)      | n == arg'.name -> return false
                    _                                        -> Nothing

          return $ (maybe Unconsumed (const Consumed) mr)
                 $ (Option <$> xs)
                    ++ (singleton $ Option $ O.Option $ x {
                          repeatable = maybe (x.repeatable) id mr
                        })

          where
            match :: Boolean -> Char -> Either SolveError O.Option
            match isTrailing f = return $
              maybe' (\_ -> O.sopt' f (toArg o.arg) (o.repeatable))
                    id
                    (head $ catMaybes $ convert f isTrailing <$> ds)

            convert :: Char -> Boolean -> Desc -> Maybe O.Option
            convert f isTrailing (Desc.OptionDesc (Desc.Option y))
              = convert' y
                where
                  convert' { name = Desc.Flag f', arg = arg' }
                    | (f == f') && (isTrailing || isNothing arg')
                    = return $ O.sopt' (f)
                                       (resolveOptArg o.arg arg')
                                       (o.repeatable)
                  convert' { name = Desc.Full f' n', arg = arg' }
                    | (f == f') && (isTrailing || isNothing arg')
                    = return $ O.opt' (Just f)
                                      (Just n')
                                      (resolveOptArg o.arg arg')
                                      (o.repeatable)
                  convert' _ = Nothing
            convert _ _ _ = Nothing

        -- | Resolve an option's argument name against that given in the
        -- | description, returning the most complete argument known.
        resolveOptArg :: Maybe String
                      -> Maybe Desc.Argument
                      -> Maybe O.Argument
        resolveOptArg (Just n) Nothing = do
          return $ O.Argument { name: n
                              , default: Nothing }
        resolveOptArg Nothing (Just (Desc.Argument a))
          = do
          -- XXX: The conversion to `StringValue` should not be needed,
          -- `Desc.Argument` should be of type `Maybe Value`.
          return $ O.Argument { name: a.name
                              , default: a.default
                              }
        resolveOptArg (Just an) (Just (Desc.Argument a))
          = do
          -- XXX: Do we need to guard that `an == a.name` here?
          -- XXX: The conversion to `StringValue` should not be needed,
          -- `Desc.Argument` should be of type `Maybe Value`.
          return $ O.Argument { name: a.name
                              , default: a.default
                              }
        resolveOptArg _ _ = Nothing

        toArg:: Maybe String -> Maybe O.Argument
        toArg a = do
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
              return $ Str.toUpper an == Str.toUpper an'
            )

solveUsage :: U.Usage -> List Desc -> Either SolveError Usage
solveUsage (U.Usage _ bs) ds = Usage <$> do
  traverse (flip solveBranch ds) bs

solve :: (List U.Usage)
      -> (List Desc)
      -> Either SolveError (List Usage)
solve us ds = traverse (flip solveUsage ds) us
