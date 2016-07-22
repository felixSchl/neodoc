module Language.Docopt.Trans.Key (
    key
  , toKeys
  , Key(..)
  ) where

import Prelude
import Data.Maybe (maybe)
import Data.String (singleton) as String
import Data.Foldable (intercalate)
import Data.Array (fromFoldable) as Array
import Data.Function (on)
import Data.String.Ext ((^=), (~~))
import Language.Docopt.Argument (Argument(..)) as D
import Language.Docopt.OptionAlias (OptionAlias(..)) as OptionAlias

newtype Key = Key { arg :: D.Argument }

instance showKey :: Show Key where
  show (Key { arg: (D.Option o) }) =
    intercalate ", " $ o.aliases <#> case _ of
      OptionAlias.Long  n -> "--" ~~ n
      OptionAlias.Short c ->  "-" ~~ String.singleton c
  show (Key { arg: (D.Positional pos) }) = pos.name
  show (Key { arg: (D.Command cmd) })    = cmd.name
  show _                                 = "invalid" -- XXX

instance ordKey :: Ord Key where
  compare = compare `on` show -- XXX

instance eqKey :: Eq Key where
  eq (Key { arg: arg0 }) (Key { arg: arg1 }) = go arg0 arg1
    where
      go (D.Command    cmd) (D.Command    cmd') = cmd.name == cmd'.name
      go (D.Positional pos) (D.Positional pos') = pos.name ^= pos'.name
      go (D.Option { aliases })
         (D.Option { aliases: aliases' }) = aliases == aliases'
      go a b = a == b

key :: D.Argument -> Key
key arg = Key { arg: arg }

-- | Derive a key from an argument.
-- | This key is what the user will use to check the value of
-- | a mathed argument.
toKeys :: D.Argument -> Array String
toKeys (D.Command cmd)    = [ cmd.name ]
toKeys (D.Positional pos) = [ pos.name ]
toKeys (D.Group _)        = []
toKeys (D.EOA)            = ["--"]
toKeys (D.Stdin)          = ["-"]
toKeys (D.Option o)       = Array.fromFoldable $ o.aliases <#> case _ of
                              OptionAlias.Long  n -> "--" ~~ n
                              OptionAlias.Short c ->  "-" ~~ String.singleton c
