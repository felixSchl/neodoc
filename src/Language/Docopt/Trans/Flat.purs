module Language.Docopt.Trans.Flat (
  reduce
  ) where

import Prelude
import Data.List (List())
import Data.StrMap (StrMap())
import Language.Docopt.Env (Env)
import Language.Docopt.Value (Value())
import Language.Docopt.RichValue (unRichValue)
import Language.Docopt.Usage (Usage()) as D
import Language.Docopt.Trans.Rich (reduce) as Rich
import Language.Docopt.ParserGen (ValueMapping)
import Language.Docopt.Argument (Branch()) as D

reduce :: List D.Usage       -- ^ the program specification
       -> Env                -- ^ the environment
       -> D.Branch           -- ^ the matched specification
       -> List ValueMapping  -- ^ the parse result
       -> StrMap Value       -- ^ the output set of (arg => val)
reduce us env b vs = (_.value <<< unRichValue) <$> Rich.reduce us env b vs
