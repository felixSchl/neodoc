module Neodoc.Data.Chunk where

import Prelude
import Data.List (List(..), singleton, (:), reverse)
import Data.Foldable (foldl, intercalate)
import Data.Pretty (class Pretty, pretty)

-- | Auxiliary data structure to indiciate whether or not the contained elements
-- | are "fixed" in space or are freely interchangable in position.

data Chunk a = Free a | Fixed a

instance showClump :: (Show a) => Show (Chunk a) where
  show (Fixed a) = "Fixed " <> show a
  show (Free  a) = "Free "  <> show a

instance prettyClump :: (Pretty a) => Pretty (Chunk (List a)) where
  pretty (Fixed as) = "<! " <> (intercalate " " $ pretty <$> as) <> " !>"
  pretty (Free  as) = "<* " <> (intercalate " " $ pretty <$> as) <> " *>"

isFree :: ∀ a. Chunk a -> Boolean
isFree (Free _) = true
isFree _        = false

getElem :: ∀ a. Chunk a -> a
getElem (Free a) = a
getElem (Fixed a) = a

chunk
  :: ∀ a
   . (a -> Boolean) -- Is `a` considered "free"?
  -> List a         -- The list of things to chunk
  -> List (Chunk (List a))
chunk isFree xs = reverse $ foldl go Nil xs
  where
  go (Nil) x = pure $ (if isFree x then Free else Fixed) $ singleton x
  go   ((Free a) :zs) x | isFree x = (Free (a <> (singleton x))) : zs
  go u@((Free _) :_ ) x = (Fixed $ singleton x) : u
  go u@((Fixed _):_ ) x | isFree x = (Free $ singleton x): u
  go   ((Fixed a):zs) x = (Fixed (a <> (singleton x))) : zs
