-- | Transform a `UsageLayout` into a `ExpandedOptionsLayout`
-- |
-- | This transform:
-- |    1. expands stacked options, ex: '-abc' into '-a -b -c'.
-- |    2. assigns option-arguments to options, ex: '--foo BAR' => '--foo=BAR'

module Neodoc.Solve.ExpandOptions (
  expandOptions
, ExpandedOptionsLayout
, ExpandedOptionsLayoutArg (..)
) where

import Data.Optimize.Uncurried
import Prelude
import Debug.Trace
import Debug.Profile
import Data.Tuple.Nested ((/\))
import Data.List (
  List(Nil), (:), filter, last, init, singleton, fromFoldable, mapMaybe
, catMaybes, head)
import Control.Alt ((<|>))
import Control.Bind (join)
import Control.Plus (empty)
import Data.Generic
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
import Data.String (Pattern(..))
import Data.String.Unsafe as US
import Data.String.Ext (endsWith)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NonEmpty
import Data.NonEmpty (singleton) as NonEmpty
import Control.MonadPlus (guard)
import Control.Extend (duplicate)
import Neodoc.Spec
import Neodoc.Spec as Spec
import Neodoc.OptionAlias as OptionAlias
import Neodoc.Data.Description
import Neodoc.Data.Layout
import Neodoc.Data.Layout as Layout
import Neodoc.Data.OptionArgument
import Neodoc.Data.UsageLayout
import Neodoc.Data.SolvedLayout
import Neodoc.Data.UsageLayout as Usage
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Solve.Traverse
import Neodoc.Solve.Error
import Partial.Unsafe (unsafePartial)

type ExpandedOptionsLayout = Layout ExpandedOptionsLayoutArg
data ExpandedOptionsLayoutArg
  = SolvedArg SolvedLayoutArg
  | ReferenceArg String

derive instance eqPreSolvedLayoutArg :: Eq ExpandedOptionsLayoutArg
derive instance genericPreSolvedLayoutArg :: Generic ExpandedOptionsLayoutArg

instance showPreSolvedLayoutArg :: Show ExpandedOptionsLayoutArg where
  show = gShow

instance prettyPreSolvedLayoutArg :: Pretty ExpandedOptionsLayoutArg where
  pretty (SolvedArg a) = pretty a
  pretty (ReferenceArg n) = "[" <> n <> "]"

expandOptions
  :: Spec UsageLayout
  -> Either SolveError (Spec ExpandedOptionsLayout)
expandOptions (Spec (spec@{ layouts, descriptions })) = do
  layouts' <- for layouts (traverse preSolveBranch)
  Right (Spec $ spec { layouts = layouts' })

  where
  preSolveBranch
    :: Layout.Branch UsageLayoutArg
    -> Either SolveError (Layout.Branch ExpandedOptionsLayoutArg)
  preSolveBranch branch = zipTraverseM preSolveAdjacent branch

  preSolveAdjacent
    :: UsageLayout
    -> Maybe UsageLayout
    -> Either SolveError (Tuple (NonEmpty List ExpandedOptionsLayout) (Maybe UsageLayout))
  preSolveAdjacent layout mAdjLayout =
    let _return xs = Right (xs /\ mAdjLayout)
        _slurp  xs = Right (xs /\ Nothing)
        mAdjArg    = mAdjLayout >>= case _ of
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
            Usage.Command    n b -> solved $ Solved.Command    n b
            Usage.Positional n b -> solved $ Solved.Positional n b
            Usage.EOA            -> solved Solved.EOA
            Usage.Stdin          -> solved Solved.Stdin
            Usage.Reference n    -> return (ReferenceArg n)
            Usage.Option n arg r -> preSolveOption (Args6 mAdjLayout slurped solved n arg r)
            Usage.OptionStack cs@(x:|xs) arg r ->
              preSolveOptionStack (Args6 mAdjLayout slurpedM solvedM cs arg r)

    where
    preSolveOption (Args6 mAdjLayout slurped solved n mArg r) = do
      mDescription <- lookupValidDescription
      case mArg of
        Just (OptionArgument aN aO) ->
          solved $ Solved.Option
            (OptionAlias.Long n)
            (Just $ OptionArgument aN aO)
            r

        Nothing -> do
          case mDescription of
            Just (_ /\ (descArg@(Just (OptionArgument aN' aO')))) -> do
              maybe
                (if not aO'
                  then fail $ "Option-Argument specified in options-section missing"
                              <> " --" <> n
                  else solved $ Solved.Option (OptionAlias.Long n) descArg r
                )
                (\(adjR /\ adjN /\ adjO) -> do
                  guardArgNames adjN aN'
                  slurped $ Solved.Option
                    (OptionAlias.Long n)
                    (Just $ OptionArgument adjN adjO)
                    adjR)
                do
                  guard (not r)
                  adjLayout <- mAdjLayout
                  case adjLayout of
                    Elem (Usage.Positional n r) -> Just (r /\ n /\ false)
                    Elem (Usage.Command    n r) -> Just (r /\ n /\ false)
                    Group o r ((x :| Nil) :| Nil) -> case x of
                      Elem (Usage.Positional n r') -> Just ((r || r') /\ n /\ o)
                      Elem (Usage.Command    n r') -> Just ((r || r') /\ n /\ o)
                      _ -> Nothing
                    _ -> Nothing
            _ -> solved $ Solved.Option (OptionAlias.Long n) Nothing r

      where
      guardArgNames aN aN' | aN ^=^ aN' = Right true
      guardArgNames aN aN' = fail
        $ "Arguments mismatch for option --" <> n <> ": "
            <> show aN <> " and " <> show aN'

      lookupValidDescription :: Either SolveError (Maybe (Tuple Boolean (Maybe OptionArgument)))
      lookupValidDescription =
        let matches = filter isMatch descriptions
         in case matches of
              Nil -> Right Nothing
              (OptionDescription _ r a _ _) : Nil -> Right (Just (r /\ a))
              _ -> fail $ "Multiple option descriptions for "
                              <> "option --" <> n
        where
        isMatch (OptionDescription aliases _ _ _ _)
          = flip any aliases case _ of
              OptionAlias.Long n' -> n == n'
              _                   -> false
        isMatch _ = false

    preSolveOptionStack (Args6 mAdjLayout slurped solved (css@(c :| cs)) mArg r) = do
      -- transform: the last stacked char is the one to receive the explicit
      -- argument binding. the rest will be w/o any binding at all.
      -- ex: -abcdef=foo -> -a -b -c -d -e -f=foo
      h :| ts <- Right case (Array.last cs) /\ (Array.init cs) of
        Just t /\ Just i -> t :| c `Array.cons` i
        _                -> c :| []

      case mArg of
        Just (arg@(OptionArgument aN aO)) -> do
          lookupValidDescription true h
          leading <- fromFoldable <$> for ts \t -> do
            lookupValidDescription false t
            Right $ Solved.Option
                    (OptionAlias.Short t)
                    Nothing
                    r
          let opt = Solved.Option (OptionAlias.Short h)
                                  (Just $ OptionArgument aN aO)
                                  r
          solved case leading of
            x : xs -> x :| xs <> singleton opt
            Nil    -> opt :| Nil

        -- this option does not have an explicit option-argument binding.
        -- however, a binding might result from either subsumption of the
        -- option's own characters or from an adjacent argument where possible.
        -- this implementation prefers subsumption over slurping.
        Nothing -> trySubsume <|> trySlurp (Args2 h ts)

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
      trySubsume = profileS "trySubsume" \_-> do
        let fs  = S.fromCharArray $ c `Array.cons` cs
        -- XXX: Purescript is not lazy, so this is too expensive.
        --      We could just stop at the first `Just` value.
        match <- head <<< catMaybes <$> for descriptions case _ of
          OptionDescription aliases _ (Just (OptionArgument aN aO)) _ _ -> do
            head <<< catMaybes <<< NonEmpty.toList <$> for aliases case _ of
              OptionAlias.Short f -> Right do
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
                          opt = Solved.Option (OptionAlias.Short f) (Just (OptionArgument aN aO)) r
                        in Just (rest /\ opt)
                        else Nothing
                  else Nothing

                -- all of the remaining options must pass the
                -- `lookupValidDescription` check, otherwise we bail out.
                rest <- either (const Nothing) (Just <<< id) do
                  fromFoldable <$> for rest \c -> do
                    lookupValidDescription false c
                    -- set the same repeatability flag for each stacked option
                    -- as indicated by trailing option.
                    Right $ Solved.Option (OptionAlias.Short c)
                                          Nothing
                                          r
                Just $ rest /\ out
              _ -> Right Nothing
          _ -> Right Nothing
        case match of
          Nothing -> fail "No description subsumed option"
          Just (rest /\ out) -> solved case rest of
            o : os -> o   :| os <> singleton out
            Nil    -> out :| Nil

      trySlurp (Args2 h ts) = do
        mDesc <- lookupValidDescription true h
        leading <- fromFoldable <$> for ts \t -> do
          lookupValidDescription false t
          -- note: return a function to override the stacked option's
          -- repeatability later.
          Right $ \r' -> Solved.Option (OptionAlias.Short t) Nothing (r || r')
        case mDesc of
          Just (_ /\ (descArg@(Just (OptionArgument aN' aO')))) -> do
            maybe'
              (\_->
                if not aO' then
                  fail $ "Option-Argument specified in options-section missing"
                        <> " -" <> S.singleton h
                else
                  let leading' = (_ $ false) <$> leading
                      opt = Solved.Option (OptionAlias.Short h) descArg r
                  in solved case leading' of
                        x : xs -> x :| xs <> singleton opt
                        Nil    -> opt :| Nil
              )
              (\(adjR /\ adjN /\ adjO) -> do
                guardArgNames adjN aN'
                let
                  leading' = (_ $ adjR) <$> leading
                  opt = Solved.Option (OptionAlias.Short h)
                                      (Just $ OptionArgument adjN adjO)
                                      adjR -- XXX: OR-apply description's repeatability here?
                slurped case leading' of
                  x : xs -> x :| xs <> singleton opt
                  Nil    -> opt :| Nil
              )
              do
                guard (not r)
                adjLayout <- mAdjLayout
                case adjLayout of
                  Elem (Usage.Positional n r) -> Just (r /\ n /\ false)
                  Elem (Usage.Command    n r) -> Just (r /\ n /\ false)
                  Group o r ((x :| Nil) :| Nil) -> case x of
                    Elem (Usage.Positional n r') -> Just ((r || r') /\ n /\ o)
                    Elem (Usage.Command    n r') -> Just ((r || r') /\ n /\ o)
                    _ -> Nothing
                  _ -> Nothing
          _ ->
            let leading' = (_ $ false) <$> leading
                opt = Solved.Option (OptionAlias.Short h) Nothing r
             in solved case leading' of
                  x : xs -> x :| xs <> singleton opt
                  Nil    -> opt :| Nil

        where
        guardArgNames aN aN' | aN ^=^ aN' = Right true
        guardArgNames aN aN' = fail
          $ "Arguments mismatch for option -" <> S.singleton c <> ": "
              <> show aN <> " and " <> show aN'

      lookupValidDescription
        :: Boolean -- is trailing?
        -> Char    -- the Char to match
        -> Either SolveError (Maybe (Tuple Boolean (Maybe OptionArgument)))
      lookupValidDescription isTrailing c =
        let matches = filter isMatch descriptions
         in case matches of
              Nil -> Right Nothing
              (OptionDescription _ r (Just a) _ _) : Nil ->
                if isTrailing
                  then Right (Just (r /\ (Just a)))
                  else fail
                    $ "Stacked option -" <> S.singleton c
                        <> " may not specify arguments"
              (OptionDescription _ r Nothing _ _) : Nil ->
                Right (Just (r /\ Nothing))
              _ -> fail $ "Multiple option descriptions for option -" <> S.singleton c
        where
        isMatch (OptionDescription aliases _ _ _ _)
          = flip any aliases case _ of
              OptionAlias.Short c' -> c == c'
              _                    -> false
        isMatch _ = false

posArgsEq :: String -> String -> Boolean
posArgsEq = eq `on` (S.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (S.stripPrefix (Pattern "<") s)
  stripSuffix s = fromMaybe s (S.stripSuffix (Pattern ">") s)


