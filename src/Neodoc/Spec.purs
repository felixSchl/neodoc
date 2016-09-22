module Neodoc.Spec where

import Prelude
import Data.List (List, null)
import Data.Maybe (Maybe(..))
import Data.Foldable (intercalate)
import Data.Pretty (class Pretty, pretty)
import Data.NonEmpty (NonEmpty, (:|))
import Neodoc.Data.Layout
import Neodoc.Data.Description

type Branch a = NonEmpty List a
type Toplevel a = List (Branch a)
newtype Spec a = Spec {
  program :: String
, layouts :: NonEmpty List (Toplevel a)
, descriptions :: List Description
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
    <> (if null descriptions then "" else "\noptions:")
    <> (intercalate "\n" $ pretty <$> descriptions)
    where prettyTopLevel t = intercalate " | " $ prettyBranch <$> t
          prettyBranch   b = intercalate " " $ pretty <$> b
