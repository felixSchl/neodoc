-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.
-- |
-- | ===
-- |
-- | Thoughts:
-- |    * It appears there is never a reason to fail hard. It would be nice if
-- |      we could produce warnings, however -> Write monad?

module Docopt.Spec.Solver where

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
import Control.Plus (empty)
import Data.Monoid (mempty)
import qualified Data.Array as A

import Docopt (Argument(..), Application(..), Branch(..), OptionArgument(..)
              , Value(..), SolveError(..))
import qualified Docopt.Spec.Parser.Desc  as D
import qualified Docopt.Spec.Parser.Usage as U

data Result = Consumed (List Argument) | Unconsumed (List Argument)

solveBranch :: U.Branch -> List D.Desc -> Either SolveError Branch
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

        solveArgs (U.Command s) _
          = Unconsumed <<< singleton <$> return (Command s)

        solveArgs (U.Positional s r) _
          = Unconsumed <<< singleton <$> return (Positional s r)

        solveArgs (U.Group o bs r) _
          = Unconsumed <<< singleton <$> do
            flip (Group o) r <$> do
              flip solveBranch ds `traverse` bs

        solveArgs o@(U.Option n a r) y = do

          -- XXX: Is `head` the right thing to do here? What if there are more
          -- matches? That would indicate ambigiutiy and needs to be treated,
          -- possibly with an error?
          let opt = flip maybe' id
                      (\_ -> Option Nothing (Just n) (toArg a) r)
                      (head $ catMaybes $ convert <$> ds)

          return $ (case y of
            Just (U.Positional n _) ->
              case opt of
                (Option _ _ (Just (OptionArgument n' _)) _)
                  | n == n' -> Consumed
                _ -> Unconsumed
            Just (U.Command n) ->
              case opt of
                (Option _ _ (Just (OptionArgument n' _)) _)
                  | n == n' -> Consumed
                _ -> Unconsumed
            _                                  -> Unconsumed
            ) $ singleton opt

          where
            convert :: D.Desc -> Maybe Argument
            convert (D.OptionDesc (D.Option { name=D.Long n', arg=a' }))
              | n' == n
              = return $ Option Nothing (Just n) (resolveOptArg a a') r
            convert (D.OptionDesc (D.Option { name=D.Full f n', arg=a' }))
              | n' == n
              = return $ Option (Just f) (Just n) (resolveOptArg a a') r
            convert _ = Nothing

        solveArgs o@(U.OptionStack f fs a r) y = do
          -- Figure out trailing flag, in order to couple it with an adjacent
          -- option where needed.
          let fs' = toList fs
              x   = case last fs' of
                      Just f' -> Tuple (f:(fromJust $ init fs')) f'
                      Nothing -> Tuple Nil f
              fs'' = fst x
              f''  = snd x

          xs <- match false `traverse` fs''
          x  <- match true f''

          return $ (case y of
            Just (U.Positional n _) ->
              case x of
                (Option _ _ (Just (OptionArgument n' _)) _)
                  | n == n' -> Consumed
                _ -> Unconsumed
            Just (U.Command n) ->
              case x of
                (Option _ _ (Just (OptionArgument n' _)) _)
                  | n == n' -> Consumed
                _ -> Unconsumed
            _ -> Unconsumed
            ) $ xs ++ singleton x

          where
            match :: Boolean -> Char -> Either SolveError Argument
            match isTrailing f = do
              return $ flip maybe' id
                        (\_ -> Option (Just f) Nothing (toArg a) r)
                        (head $ catMaybes $ convert f isTrailing <$> ds)

            convert :: Char -> Boolean -> D.Desc -> Maybe Argument
            convert f isTrailing (D.OptionDesc (D.Option { name=D.Flag f', arg=a' }))
              | (f == f')
                && (isTrailing || isNothing a')
              = return $ Option (Just f) Nothing (resolveOptArg a a') r
            convert f isTrailing (D.OptionDesc (D.Option { name=D.Full f' n, arg=a' }))
              | (f == f')
                && (isTrailing || isNothing a')
              = return $ Option (Just f) (Just n) (resolveOptArg a a') r
            convert _ _ _ = Nothing

        -- | Resolve an option's argument name against that given in the
        -- | description, returning the most complete argument known.
        resolveOptArg :: Maybe String
                      -> Maybe D.Argument
                      -> Maybe OptionArgument
        resolveOptArg (Just n) Nothing = return $ OptionArgument n Nothing
        resolveOptArg Nothing (Just (D.Argument a))
          = do
          -- XXX: The conversion to `StringValue` should not be needed,
          -- `Desc.Argument` should be of type `Maybe Value`.
          return $ OptionArgument a.name (StringValue <$> a.default)
        resolveOptArg (Just an) (Just (D.Argument a))
          = do
          -- XXX: Do we need to guard that `an == a.name` here?
          -- XXX: The conversion to `StringValue` should not be needed,
          -- `Desc.Argument` should be of type `Maybe Value`.
          return $ OptionArgument a.name (StringValue <$> a.default)
        resolveOptArg _ _ = Nothing

        toArg:: Maybe String -> Maybe OptionArgument
        toArg a = a >>= \an -> return $ OptionArgument an Nothing

solveUsage :: U.Usage -> List D.Desc -> Either SolveError Application
solveUsage (U.Usage _ bs) ds = Application <$> (foldM step Nil bs)
  where
    step :: List Branch -> U.Branch -> Either SolveError (List Branch)
    step bs b = do
      x <- solveBranch b ds
      return $ bs ++ (singleton x)

solve :: (List U.Usage)
      -> (List D.Desc)
      -> Either SolveError (List Application)
solve us ds = foldM step Nil us
  where
    step :: List Application -> U.Usage -> Either SolveError (List Application)
    step as u = do
      x <- solveUsage u ds
      return $ as ++ (singleton x)
