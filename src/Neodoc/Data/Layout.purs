module Neodoc.Data.Layout where

import Prelude
import Data.Foldable (intercalate)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty.Extra as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Function (on)
import Data.Tuple.Nested ((/\))
import Data.Pretty (class Pretty, pretty)
import Data.List (List(..), (:))
import Data.Foldable (find)
import Data.Foldable.Extra (findMap)
import Data.NonEmpty (NonEmpty())
import Data.Foreign (F)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Neodoc.Data.EmptyableLayout

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

getElem :: ∀ a. Partial => Layout a -> a
getElem (Elem x) = x

findElem :: ∀ a. (a -> Boolean) -> Layout a -> Maybe a
findElem f (Elem x) | f x = Just x
findElem f (Group _ _ xs) = case NonEmpty.concat xs of x:|xs -> go (x:xs)
  where
  go Nil = Nothing
  go (x:xs) = findElem f x <|> go xs
findElem _ _ = Nothing

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
  show (Elem  x)      = "Elem " <> show x
  show (Group o r xs) = "Group " <> show o <> " " <> show r <> " " <> show xs

derive instance eqLayout :: (Eq a) => Eq (Layout a)
derive instance ordLayout :: (Ord a) => Ord (Layout a)
