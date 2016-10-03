module Neodoc.Spec where

import Prelude
import Data.List (List(..), (:), null, fromFoldable, catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Foldable (intercalate)
import Data.Traversable (sequence)
import Data.Pretty (class Pretty, pretty)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Foreign (Foreign)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.Index as F
import Data.Foreign.Index ((!))
import Data.Foreign.Class
import Data.Foreign.Extra as F
import Data.Foreign (F)
import Control.Alt ((<|>))
import Neodoc.Data.Layout
import Neodoc.Data.Description

type Branch a = NonEmpty List a
type Toplevel a = List (Branch a)
newtype Spec a = Spec {
  program :: String
, layouts :: NonEmpty List (Toplevel a)
, descriptions :: List Description
, helpText :: String
, shortHelp :: String
}

instance isForeignOptionAlias :: (IsForeign a) => IsForeign (Spec a) where
  read v = Spec <$> do
    { program: _, layouts:_, descriptions:_, helpText:_, shortHelp:_ }
      <$> F.readProp "program" v
      <*> readLayouts v
      <*> readDescriptions v
      <*> F.readProp "helpText" v
      <*> F.readProp "shortHelp" v
    where

    readLayouts v = lmap (F.errorAt "layouts") do
      xs :: Array Foreign <- F.readProp "layouts" v
      xs' <- fromFoldable <$>
        sequence (Array.zipWith readToplevel (zero .. (Array.length xs)) xs)
      case xs' of
        x:xs -> pure $ x :| xs
        Nil  -> pure $ Nil :| Nil

    readToplevel i v = lmap (F.ErrorAtIndex i) do
      xs :: Array Foreign <- F.read v
      catMaybes <<< fromFoldable <$>
        sequence (Array.zipWith readBranch (zero .. (Array.length xs)) xs)

    readBranch i v = lmap (F.ErrorAtIndex i) do
      xs :: Array a <- F.read v
      pure case fromFoldable xs of
        x:xs -> Just $ x :| xs
        Nil  -> Nothing

    readDescriptions v = do
      xs :: Array Description <- F.readProp "descriptions" v
      pure $ fromFoldable xs

instance asForeignOptionAlias :: (AsForeign a) => AsForeign (Spec a) where
  write (Spec spec@{ layouts, descriptions }) =
    let layouts' = Array.fromFoldable $ layouts <#> \toplevel ->
                    Array.fromFoldable $ toplevel <#> \branch ->
                      Array.fromFoldable $ F.write <$> branch
        descriptions' = Array.fromFoldable $ F.write <$> descriptions
     in F.toForeign $ spec {
          layouts = layouts'
        , descriptions = descriptions'
        }

instance eqSpec :: (Eq a) => Eq (Spec a) where
  eq (Spec { program, layouts, descriptions })
     (Spec { program: program', layouts: layouts', descriptions: descriptions' })
      = program == program' && layouts == layouts' && descriptions == descriptions'

instance showSpec :: (Show a) => Show (Spec a) where
  show (Spec { program, layouts, descriptions }) = "Spec {"
    <> " program: " <> show program
    <> ", layouts: " <> show layouts
    <> ", descriptions: " <> show descriptions
    <> " }"

instance prettySpec :: (Pretty a) => Pretty (Spec a) where
  pretty (Spec { program, layouts: t :| ts, descriptions }) =
    "usage: " <> program <> " " <> prettyTopLevel t
    <> (if null ts then "" else "\n")
    <> (intercalate "\n" $ ts <#> \t ->
        "   or: " <> program <> " " <> prettyTopLevel t)
    <> (if null descriptions then "" else "\noptions:\n")
    <> (intercalate "\n" $ pretty <$> descriptions)
    where prettyTopLevel t = intercalate " | " $ prettyBranch <$> t
          prettyBranch   b = intercalate " " $ pretty <$> b
