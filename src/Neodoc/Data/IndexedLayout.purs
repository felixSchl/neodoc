module Neodoc.Data.IndexedLayout where

import Neodoc.Data.Layout
import Neodoc.Data.Indexed

type IndexedLayout a = Layout (IndexedLayoutArg a)
type IndexedLayoutArg a = Indexed a
