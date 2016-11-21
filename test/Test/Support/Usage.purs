module Test.Support.Usage where

import Prelude
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..), length, (!!), take, fromFoldable)
import Data.NonEmpty (NonEmpty, (:|))
import Text.Wrap (dedent)
import Partial.Unsafe (unsafePartial)

import Neodoc.Data.Layout
import Neodoc.Data.UsageLayout as U
--
-- -- short hand to create a usage
-- usage :: Array (Array U.UsageLayoutArg) -> U.L
-- usage xss = fromFoldable $ fromFoldable <$> xss

listToNonEmpty :: forall a. Partial => List a -> NonEmpty List a
listToNonEmpty (x:xs) = x :| xs

-- short hand to create a required group node
gr :: forall a. Array (Array (Layout a)) -> Boolean -> Layout a
gr xs r =
  let ys = fromFoldable xs
      ys' = fromFoldable <$> ys
      ys'' = unsafePartial $ listToNonEmpty $ listToNonEmpty <$> ys'
   in Group false r ys''

go :: forall a. Array (Array (Layout a)) -> Boolean -> Layout a
go xs r =
  let ys = fromFoldable xs
      ys' = fromFoldable <$> ys
      ys'' = unsafePartial $ listToNonEmpty $ listToNonEmpty <$> ys'
   in Group true r ys''

ref n     = Elem $ U.Reference n
eoa       = Elem $ U.EOA
stdin     = Elem $ U.Stdin
co n      = Elem $ U.Command n false
coR n     = Elem $ U.Command n true
po' n r   = Elem $ U.Positional n r
po n      = po' n false
poR n     = po' n true

sopt'  f fs a r = Elem $ U.OptionStack (f:|fs) false a r
sopt   f fs a   = sopt' f fs (pure a) false
sopt_  f fs     = sopt' f fs Nothing false
soptR  f fs a   = sopt' f fs (pure a) true
soptR_ f fs     = sopt' f fs Nothing true
lopt'  n    a r = Elem $ U.Option n false a r
lopt   n    a   = lopt' n (pure a) false
lopt_  n        = lopt' n Nothing false
loptR  n    a   = lopt' n (pure a) true
loptR_ n        = lopt' n Nothing true
