module Language.Docopt.Trans.Key (
    key
  , Key(..)
  ) where

import Prelude
import Data.Maybe (maybe)
import Data.String (fromChar)
import Data.Function (on)
import Data.String.Ext ((^=))
import Language.Docopt.Option as O
import Language.Docopt.Argument (Argument(..)) as D
newtype Key = Key { arg :: D.Argument }

instance showKey :: Show Key where
  show (Key { arg: (D.Option (O.Option o)) }) =
    maybe "" (\c -> fromChar c ++ ", ") o.flag
      ++ maybe "" id o.name
  show (Key { arg: (D.Positional n _) }) = n
  show (Key { arg: (D.Command n _) }) = n
  show _ = "invalid" -- XXX

instance ordKey :: Ord Key where
  compare = compare `on` show -- XXX

instance eqKey :: Eq Key where
  eq (Key { arg: arg0 }) (Key { arg: arg1 }) = go arg0 arg1
    where
      go (D.Command    n _) (D.Command    n' _) = n == n'
      go (D.Positional n _) (D.Positional n' _) = n ^= n'
      go (D.Option (O.Option { flag=f,  name=n  }))
         (D.Option (O.Option { flag=f', name=n' }))
         = (f == f') && (n == n')
      go a b = a == b

key :: D.Argument -> Key
key arg = Key { arg: arg }


