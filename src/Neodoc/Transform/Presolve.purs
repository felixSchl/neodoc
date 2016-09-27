-- | Transform a `UsageLayout` into a `PreSolvedLayout`
-- |
-- | This transform:
-- |    1. expands stacked options, ex: '-abc' into '-a -b -c'.
-- |    2. assigns option-arguments to options, ex: '--foo BAR' => '--foo=BAR'

module Neodoc.Transform.PreSolve (
  preSolve
, PreSolvedLayout
, PreSolvedLayoutArg (..)
) where

import Prelude
import Debug.Trace
import Data.List (
  List(Nil), (:), filter, last, init, singleton, fromFoldable, mapMaybe
, catMaybes, head)
import Control.Alt ((<|>))
import Control.Bind (join)
import Control.Plus (empty)
import Data.Array as Array
import Data.Pretty (class Pretty, pretty)
import Data.Function (on)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), maybe, maybe', fromMaybe)
import Data.Either (Either(..), either)
import Data.Traversable (for, traverse)
import Data.Foldable (any)
import Data.String as S
import Data.String.Unsafe as US
import Data.String.Ext (endsWith)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty (singleton) as NonEmpty
import Control.MonadPlus (guard)
import Control.Extend (duplicate)
import Control.Monad.Trans (lift)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Except.Trans (runExceptT, ExceptT)
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.Description
import Neodoc.Data.Layout
import Neodoc.Data.Layout as Layout
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout
import Neodoc.Data.UsageLayout as Usage
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Transform.SolveError
import Partial.Unsafe (unsafePartial)

type PreSolvedLayout = Layout PreSolvedLayoutArg
data PreSolvedLayoutArg
  = SolvedArg SolvedLayoutArg
  | ReferenceArg String

instance eqPreSolvedLayoutArg :: Eq PreSolvedLayoutArg where
  eq (SolvedArg a) (SolvedArg a') = a == a'
  eq (ReferenceArg n) (ReferenceArg n') = n == n'
  eq _ _ = false

instance showPreSolvedLayoutArg :: Show PreSolvedLayoutArg where
  show (SolvedArg a) = "SolvedArg " <> show a
  show (ReferenceArg n) = "ReferenceArg " <> show n

instance prettyPreSolvedLayoutArg :: Pretty PreSolvedLayoutArg where
  pretty (SolvedArg a) = pretty a
  pretty (ReferenceArg n) = "[" <> n <> "]"

preSolve
  :: Spec UsageLayout
  -> Either SolveError (Spec PreSolvedLayout)
preSolve (Spec { program, layouts, descriptions }) = do
  layouts <- for layouts (traverse preSolveBranch)
  pure (Spec { program, layouts, descriptions })

  where
  preSolveBranch
    :: Layout.Branch UsageLayoutArg
    -> Either SolveError (Layout.Branch PreSolvedLayoutArg)
  preSolveBranch branch = zipTraverseM preSolveAdjacent branch

  preSolveAdjacent
    :: Maybe UsageLayout
    -> UsageLayout
    -> Either SolveError (Tuple (Maybe UsageLayout) (NonEmpty List PreSolvedLayout))
  preSolveAdjacent mAdjLayout layout =
    let _return xs = pure (mAdjLayout /\ xs)
        _slurp  xs = pure (Nothing    /\ xs)
        mAdjArg   = mAdjLayout >>= case _ of
                      Group _ _ _ -> Nothing
                      Elem  x     -> Just x
     in case layout of
        Group o r xs -> do
          e <- Group o r <$> traverse preSolveBranch xs
          _return $ NonEmpty.singleton e
        Elem x ->
          let returnM  xs = _return (Elem <$> xs)
              slurp    xs = _slurp  (Elem <$> xs)
              solvedM  xs = returnM (SolvedArg <$> xs)
              slurpedM xs = slurp   (SolvedArg <$> xs)
              return      = returnM  <<< NonEmpty.singleton
              solved      = solvedM  <<< NonEmpty.singleton
              slurped     = slurpedM <<< NonEmpty.singleton
          in case x of
            Usage.Command    n b       -> solved $ Solved.Command    n b
            Usage.Positional n b       -> solved $ Solved.Positional n b
            Usage.EOA                  -> solved Solved.EOA
            Usage.Stdin                -> solved Solved.Stdin
            Usage.Reference n          -> return (ReferenceArg n)
            Usage.Option n arg r       -> preSolveOption mAdjLayout slurped solved n arg r
            Usage.OptionStack cs arg r -> preSolveOptionStack mAdjLayout slurpedM solvedM cs arg r

    where
    preSolveOption mAdjLayout slurped solved n mArg r = do
      mDescription <- lookupValidDescription
      case mArg of
        Just (Usage.OptionArgument aN aO) ->
          solved $ Solved.Option
            (OptionAlias.Long n)
            (Just $ Solved.OptionArgument aN aO)
            r

        Nothing -> do
          case mDescription of
            Just (_ /\ (Just (Usage.OptionArgument aN' aO'))) -> do
              maybe
                (fail $ "Option-Argument specified in options-section missing"
                        <> " --" <> n)
                (\(adjR /\ adjN /\ adjO) -> do
                  guardArgNames adjN aN'
                  slurped $ Solved.Option
                    (OptionAlias.Long n)
                    (Just $ Solved.OptionArgument adjN adjO)
                    adjR)
                do
                  guard (not r)
                  adjLayout <- mAdjLayout
                  case adjLayout of
                    Elem (Usage.Positional n r) -> pure (r /\ n /\ false)
                    Elem (Usage.Command    n r) -> pure (r /\ n /\ false)
                    Group o r ((x :| Nil) :| Nil) -> case x of
                      Elem (Usage.Positional n r') -> pure ((r || r') /\ n /\ o)
                      Elem (Usage.Command    n r') -> pure ((r || r') /\ n /\ o)
                      _ -> Nothing
                    _ -> Nothing
            _ -> solved $ Solved.Option (OptionAlias.Long n) Nothing r

      where
      guardArgNames aN aN' | aN ^=^ aN' = pure true
      guardArgNames aN aN' = fail
        $ "Arguments mismatch for option --" <> n <> ": "
            <> show aN <> " and " <> show aN'

      lookupValidDescription :: Either SolveError (Maybe (Tuple Boolean (Maybe Usage.OptionArgument)))
      lookupValidDescription =
        let matches = filter isMatch descriptions
         in case matches of
              Nil -> pure Nothing
              (OptionDescription _ r a _ _) : Nil -> pure (Just (r /\ a))
              _ -> fail $ "Multiple option descriptions for "
                              <> "option --" <> n
        where
        isMatch (OptionDescription aliases _ _ _ _)
          = flip any aliases case _ of
              OptionAlias.Long n' -> n == n'
              _                   -> false
        isMatch _ = false

    preSolveOptionStack mAdjLayout slurped solved (css@(c :| cs)) mArg r = do
      -- transform: the last stacked char is the one to receive the explicit
      -- argument binding. the rest will be w/o any binding at all.
      -- ex: -abcdef=foo -> -a -b -c -d -e -f=foo
      h :| ts <- pure case (Array.last cs) /\ (Array.init cs) of
        Just t /\ Just i -> t :| c `Array.cons` i
        _                -> c :| []

      case mArg of
        Just (arg@(Usage.OptionArgument aN aO)) -> do
          lookupValidDescription true h
          leading <- fromFoldable <$> for ts \t -> do
            lookupValidDescription false t
            pure $ Solved.Option
                    (OptionAlias.Short t)
                    Nothing
                    r
          let opt = Solved.Option (OptionAlias.Short h)
                                  (Just $ Solved.OptionArgument aN aO)
                                  r
          solved case leading of
            x : xs -> x :| xs <> singleton opt
            Nil    -> opt :| Nil

        -- this option does not have an explicit option-argument binding.
        -- however, a binding might result from either subsumption of the
        -- option's own characters or from an adjacent argument where possible.
        -- this implementation prefers subsumption over slurping.
        Nothing -> trySubsume <|> trySlurp h ts

      where
      -- try to subsume the option stack by iterating over it, checking for each
      -- char if there's a corresponding entry in the description text and if
      -- so, check to see if the argument mentioned in the
      -- description text equals the remaining chars on the right.
      --
      -- ex:
      -- usage:       -abcdFILE
      -- description: -d FILE
      -- solved:      -a -b -c -d=FILE
      --
      -- this implementation is simple and favours the first description to
      -- yield a hit.
      trySubsume = do
        let fs  = S.fromCharArray $ c `Array.cons` cs
        -- XXX: Purescript is not lazy, so this is too expensive.
        --      We could just stop at the first `Just` value.
        match <- head <<< catMaybes <$> for descriptions case _ of
          OptionDescription aliases _ (Just (Usage.OptionArgument aN aO)) _ _ -> do
            head <<< catMaybes <<< fromFoldable <$> for aliases case _ of
              OptionAlias.Short f -> pure do
                -- the haystack needs to be modified, such that the
                -- the last (length a.name) characters are uppercased
                -- and hence compared case INSENSITIVELY.
                let bareArgname = stripAngles aN
                    needle = S.toUpper $ S.singleton f <> bareArgname
                    haystack = S.toUpper fs

                rest /\ out <- if endsWith needle haystack
                  then
                    let ix = S.length haystack - S.length needle
                     in if unsafePartial (US.charAt ix fs) == f then
                        let
                          rest = S.toCharArray $ S.take (S.length fs - S.length bareArgname - 1) fs
                          opt = Solved.Option (OptionAlias.Short f) (Just (Solved.OptionArgument aN aO)) r
                        in Just (rest /\ opt)
                        else Nothing
                  else Nothing

                -- all of the remaining options must pass the
                -- `lookupValidDescription` check, otherwise we bail out.
                rest <- either (const Nothing) (pure <<< id) do
                  fromFoldable <$> for rest \c -> do
                    lookupValidDescription false c
                    -- set the same repeatability flag for each stacked option
                    -- as indicated by trailing option.
                    pure $ Solved.Option (OptionAlias.Short c)
                                         Nothing
                                         r
                pure $ rest /\ out
              _ -> pure Nothing
          _ -> pure Nothing
        case match of
          Nothing -> fail "No description subsumed option"
          Just (rest /\ out) -> solved case rest of
            o : os -> o   :| os <> singleton out
            Nil    -> out :| Nil

      trySlurp h ts = do
        mDesc <- lookupValidDescription true h
        leading <- fromFoldable <$> for ts \t -> do
          lookupValidDescription false t
          -- note: return a function to override the stacked option's
          -- repeatability later.
          pure $ \r' -> Solved.Option (OptionAlias.Short t) Nothing (r || r')
        case mDesc of
          Just (_ /\ (Just (Usage.OptionArgument aN' aO'))) -> do
            maybe'
              (\_->
                if not aO' then
                  fail $ "Option-Argument specified in options-section missing"
                        <> " -" <> S.singleton h
                else
                  let leading' = (_ $ false) <$> leading
                      opt = Solved.Option (OptionAlias.Short h) Nothing r
                  in solved case leading' of
                        x : xs -> x :| xs <> singleton opt
                        Nil    -> opt :| Nil
              )
              (\(adjR /\ adjN /\ adjO) -> do
                guardArgNames adjN aN'
                let
                  leading' = (_ $ adjR) <$> leading
                  opt = Solved.Option (OptionAlias.Short h)
                                      (Just $ Solved.OptionArgument adjN adjO)
                                      adjR -- XXX: OR-apply description's repeatability here?
                slurped case leading' of
                  x : xs -> x :| xs <> singleton opt
                  Nil    -> opt :| Nil
              )
              do
                guard (not r)
                adjLayout <- mAdjLayout
                case adjLayout of
                  Elem (Usage.Positional n r) -> pure (r /\ n /\ false)
                  Elem (Usage.Command    n r) -> pure (r /\ n /\ false)
                  Group o r ((x :| Nil) :| Nil) -> case x of
                    Elem (Usage.Positional n r') -> pure ((r || r') /\ n /\ o)
                    Elem (Usage.Command    n r') -> pure ((r || r') /\ n /\ o)
                    _ -> Nothing
                  _ -> Nothing
          _ ->
            let leading' = (_ $ false) <$> leading
                opt = Solved.Option (OptionAlias.Short h) Nothing r
             in solved case leading' of
                  x : xs -> x :| xs <> singleton opt
                  Nil    -> opt :| Nil

        where
        guardArgNames aN aN' | aN ^=^ aN' = pure true
        guardArgNames aN aN' = fail
          $ "Arguments mismatch for option -" <> S.singleton c <> ": "
              <> show aN <> " and " <> show aN'

      lookupValidDescription
        :: Boolean -- is trailing?
        -> Char    -- the Char to match
        -> Either SolveError (Maybe (Tuple Boolean (Maybe Usage.OptionArgument)))
      lookupValidDescription isTrailing c =
        let matches = filter isMatch descriptions
         in case matches of
              Nil -> pure Nothing
              (OptionDescription _ r (Just a) _ _) : Nil ->
                if isTrailing
                  then pure (Just (r /\ (Just a)))
                  else fail
                    $ "Stacked option -" <> S.singleton c
                        <> " may not specify arguments"
              (OptionDescription _ r Nothing _ _) : Nil ->
                pure (Just (r /\ Nothing))
              _ -> fail $ "Multiple option descriptions for option -" <> S.singleton c
        where
        isMatch (OptionDescription aliases _ _ _ _)
          = flip any aliases case _ of
              OptionAlias.Short c' -> c == c'
              _                    -> false
        isMatch _ = false

zipTraverseM
  :: ∀ a b m
   . (Monad m)
  => (Maybe a -> a -> m (Tuple (Maybe a) (NonEmpty List b)))
  -> NonEmpty List a
  -> m (NonEmpty List b)
zipTraverseM f = go
  where
    go (x :| Nil) = do
      ma /\ (a :| as) <- f Nothing x
      case ma of
        Nothing -> pure (a :| as)
        Just y  -> do
          b :| bs <- go (y :| Nil)
          pure (a :| as <> (b : bs)) -- note: `as` won't be run again

    go (x :| y : xss) = do
      ma /\ (a :| as) <- f (Just y) x
      case ma of
        Nothing -> pure (a :| as)
        Just z  -> do
          b :| bs <- go (z :| xss)
          pure (a :| as <> (b : bs)) -- note: `as` won't be run again

posArgsEq :: String -> String -> Boolean
posArgsEq = eq `on` (S.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (S.stripPrefix "<" s)
  stripSuffix s = fromMaybe s (S.stripSuffix ">" s)
