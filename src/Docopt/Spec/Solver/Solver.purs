-- | Resolve ambiguities by combining the parsed usage section with any parsed
-- | Option sections, as well as some best effort guessing.
-- |
-- | ===

module Docopt.Spec.Solver where

import Prelude
import Debug.Trace
import Data.Either (Either(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe (Maybe(..), isJust, maybe, maybe', isNothing)
import Data.List (List(..), filter, head, foldM, concat, (:), singleton
                , catMaybes, toList, last, init, length)
import Data.Traversable (traverse)
import Data.Foldable (foldl)
import Control.Plus (empty)
import Data.Monoid (mempty)
import qualified Data.Array as A

import Docopt (Argument(..), Application(..), Branch(..), OptionArgument(..)
              , Value(..))
import qualified Docopt.Spec.Parser.Desc  as D
import qualified Docopt.Spec.Parser.Usage as U

data SolveError = SolveError
instance showSolveError :: Show SolveError where
  show _ = "SolveError"

solveArg :: U.Argument -> List D.Desc -> Either SolveError (List Argument)
solveArg (U.Command s) _       = singleton <$> return (Command s)
solveArg (U.Positional s r) _  = singleton <$> return (Positional s r)
solveArg o@(U.Option n a r) ds = singleton <$> do
  -- XXX: Is `head` the right thing to do here? What if there are more
  -- matches? That would indicate ambigiutiy and needs to be treated, possibly
  -- with an error?
  return $ maybe' (\_ -> Option Nothing (Just n) (toArg a) r)
                  id
                  (head $ catMaybes $ convert <$> ds)

  where
    toArg:: Maybe String -> Maybe OptionArgument
    toArg a = a >>= \an -> return $ OptionArgument an Nothing

    resolveArg :: Maybe String -> Maybe D.Argument -> Maybe OptionArgument
    resolveArg (Just an) Nothing = return $ OptionArgument an Nothing
    resolveArg Nothing (Just (D.Argument a))
      -- XXX: The conversion to `StringValue` should not be needed,
      -- `Desc.Argument` should be of type `Maybe Value`.
      = return $ OptionArgument a.name (StringValue <$> a.default)
    resolveArg (Just an) (Just (D.Argument a))
      -- XXX: Do we need to guard that `an == a.name` here?
      -- XXX: The conversion to `StringValue` should not be needed,
      -- `Desc.Argument` should be of type `Maybe Value`.
      = return $ OptionArgument a.name (StringValue <$> a.default)
    resolveArg _ _ = Nothing

    convert :: D.Desc -> Maybe Argument
    convert (D.OptionDesc (D.Option { name=D.Long n', arg=a' }))
      | n' == n
      = return $ Option Nothing (Just n) (resolveArg a a') r
    convert (D.OptionDesc (D.Option { name=D.Full f n', arg=a' }))
      | n' == n
      = return $ Option (Just f) (Just n) (resolveArg a a') r
    convert _ = Nothing

solveArg o@(U.OptionStack f fs a r) ds = do
  let fs' = f:toList fs

  -- Ensure that only the last stacked option is to be considered in
  -- "trailing" position and hence capable of expanding to take an argument.
  if length fs' > 1
    then do
      xs <- traverse (match false) (fromJust $ init fs')
      x  <- match true (fromJust $ last fs')
      return $ xs ++ singleton x
    else
      traverse (match true) fs'

  where
    match :: Boolean -> Char -> Either SolveError Argument
    match isTrailing f = do
      return $ maybe' (\_ -> Option (Just f) Nothing (toArg a) r)
                      id
                      (head $ catMaybes $ convert f isTrailing <$> ds)

    toArg:: Maybe String -> Maybe OptionArgument
    toArg a = a >>= \an -> return $ OptionArgument an Nothing

    resolveArg :: Maybe String -> Maybe D.Argument -> Maybe OptionArgument
    resolveArg (Just an) Nothing = return $ OptionArgument an Nothing
    resolveArg Nothing (Just (D.Argument a))
      -- XXX: The conversion to `StringValue` should not be needed,
      -- `Desc.Argument` should be of type `Maybe Value`.
      = return $ OptionArgument a.name (StringValue <$> a.default)
    resolveArg (Just an) (Just (D.Argument a))
      -- XXX: Do we need to guard that `an == a.name` here?
      -- XXX: The conversion to `StringValue` should not be needed,
      -- `Desc.Argument` should be of type `Maybe Value`.
      = return $ OptionArgument a.name (StringValue <$> a.default)

    convert :: Char -> Boolean -> D.Desc -> Maybe Argument
    convert f isTrailing (D.OptionDesc (D.Option { name=D.Flag f', arg=a' }))
      | (f == f')
        && (isTrailing || isNothing a')
      = return $ Option (Just f) Nothing (resolveArg a a') r
    convert f isTrailing (D.OptionDesc (D.Option { name=D.Full f' n, arg=a' }))
      | (f == f')
        && (isTrailing || isNothing a')
      = return $ Option (Just f) (Just n) (resolveArg a a') r
    convert _ _ _ = Nothing

solveArg (U.Group o bs r) ds  = singleton <$> do
  flip (Group o) r <$> do
    foldM go empty bs
      where go :: List Branch -> U.Branch -> Either SolveError (List Branch)
            go a b = do
              br <- solveBranch b ds
              return (br:a)

solveBranch :: U.Branch -> List D.Desc -> Either SolveError Branch
solveBranch as ds = Branch <<< concat <$> (foldM step Nil as)
  where
    step :: List (List Argument)
          -> U.Argument
          -> Either SolveError (List (List Argument))
    step ass a = do
      xs <- solveArg a ds
      return $ ass ++ (singleton xs)

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
