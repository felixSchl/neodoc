module Language.Docopt.Pretty where

import Prelude
import Data.Maybe (maybe)
import Data.Foldable (intercalate)
import Data.Monoid (Monoid)
import Data.String (fromChar)
import Control.Apply ((*>))
import Language.Docopt.Types

prettyPrintArg :: Argument -> String
prettyPrintArg (EOA)               = "--"
prettyPrintArg (Command name)      = name
prettyPrintArg (Positional name r) = name ++ (if r then "..." else "")
prettyPrintArg (Option flag name arg r)
  = short ++ long ++ arg' ++ rep ++ default
  where
    short   = maybe "" (\f -> "-" ++ (fromChar f)) flag
    long    = maybe "" (const ", ") (flag *> name) ++ maybe "" ("--" ++) name
    rep     = if r then "..." else ""
    arg'    = flip (maybe "") arg \(OptionArgument n _) -> "="  ++ n
    default = flip (maybe "") arg \(OptionArgument _ d) ->
                flip (maybe "") d \d' ->
                  " [default: " ++ (prettyPrintValue d') ++  "]"

prettyPrintArg (Group o bs r) = open ++ inner ++ close ++ repetition
  where
    open       = if o then "[" else "("
    close      = if o then "]" else ")"
    inner      = intercalate " | " (prettyPrintBranch <$> bs)
    repetition = if r then "..." else ""

prettyPrintBranch :: Branch -> String
prettyPrintBranch (Branch xs) = intercalate " " (prettyPrintArg <$> xs)

prettyPrintUsage :: Usage -> String
prettyPrintUsage (Usage xs)
  = intercalate " | " (prettyPrintBranch <$> xs)

prettyPrintValue :: Value -> String
prettyPrintValue (StringValue s) = s
prettyPrintValue (BoolValue b)   = show b
prettyPrintValue (ArrayValue xs) = show $ prettyPrintValue <$> xs
