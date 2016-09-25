module Neodoc.Data.Layout where

import Prelude
import Data.Foldable (intercalate)
import Data.Function (on)
import Data.Pretty (class Pretty, pretty)
import Data.List (List())
import Data.NonEmpty (NonEmpty())

{-
the general structure of a comamnd line:
  (ELEM ... [| ELEM ...)
-}
type Branch a = NonEmpty List (Layout a)
data Layout a
  = Group
      Boolean -- optional
      Boolean -- repeatable
      (NonEmpty List (Branch a))
  | Elem a

instance functorLayout :: Functor Layout where
  map f (Group o r xs) = Group o r $ ((f <$> _) <$> _) <$> xs
  map f (Elem x)       = Elem (f x)

instance prettyLayout :: (Pretty a) => Pretty (Layout a) where
  pretty (Elem  x) = pretty x
  pretty (Group o r xs) =
    let inner = intercalate "|" $ xs <#> \x -> intercalate " " $  pretty <$> x
     in (if o then "[" else "(")
      <> inner
      <> (if o then "]" else ")")
      <> (if r then "..." else "")

instance showLayout :: (Show a) => Show (Layout a) where
  show (Elem  x) = "Elem " <> show x
  show (Group o r xs) = "Group " <> show o <> " " <> show r <> " " <> show xs

instance eqLayout :: (Eq a) => Eq (Layout a) where
  eq (Elem x) (Elem x') = x == x'
  eq (Group o r xs) (Group o' r' xs') = o == o' && r == r' && xs == xs'
  eq _ _ = false

-- XXX: implement this properly
instance ordLayout :: (Show a, Eq a) => Ord (Layout a) where
  compare = compare `on` show
