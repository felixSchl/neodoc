module Neodoc.Spec.Token where

import Prelude
import Data.Pretty
import Data.Array as A
import Data.NonEmpty ((:|), NonEmpty)
import Data.NonEmpty as NonEmpty
import Data.NonEmpty.Extra as NonEmpty
import Data.String (fromCharArray)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Either (Either(..))
import Data.List (List)
import Neodoc.Spec.Token
import Neodoc.Parsing.Parser.Pos as P

data PositionedToken = PositionedToken P.Position Token

instance prettyPositionedToken :: Pretty PositionedToken where
  pretty (PositionedToken _ tok) = pretty tok

instance showPositionedToken :: Show PositionedToken where
  show (PositionedToken pos tok) = "PositionedToken " <> show pos <> " " <> show  tok

-- XXX: newtype this
type OptionArgument = {
  name     :: String
, optional :: Boolean
}

data Polarity = Positive | Negative | Both
derive instance eqPolarity :: Eq Polarity

data Token
  = LParen
  | RParen
  | LSquare
  | RSquare
  | Dash
  | VBar
  | Colon
  | Comma
  | Newline
  | TripleDot
  | Reference String
  | LOpt String Polarity (Maybe OptionArgument)
  | SOpt (NonEmpty Array Char) Polarity (Maybe OptionArgument)
  | Tag String String
  | Name String
  | ShoutName String
  | AngleName String
  | Garbage Char
  | DoubleDash

instance showToken :: Show Token where
  show = pretty --- XXX: TEMP

instance prettyToken :: Pretty Token where
  pretty LParen        = show '('
  pretty RParen        = show ')'
  pretty LSquare       = show '['
  pretty RSquare       = show ']'
  pretty Dash          = show '-'
  pretty VBar          = show '|'
  pretty Newline       = show '\n'
  pretty Colon         = show ':'
  pretty Comma         = show ','
  pretty TripleDot     = "..."
  pretty DoubleDash    = "--"
  pretty (Reference r) = "Reference " <> show r
  pretty (Garbage   c) = "Garbage "   <> show c
  pretty (Tag k v)     = "Tag "       <> show k <> " "  <> show v
  pretty (Name      n) = "Name "      <> show n
  pretty (ShoutName n) = "ShoutName " <> show n
  pretty (AngleName n) = "AngleName " <> show n
  pretty (LOpt n pol arg)  = sign <> n <> arg'
    where sign = case pol of
                  Positive -> "--"
                  Negative -> "--no-"
                  Both     -> "--[no-]"
          arg' = fromMaybe "" do
                  arg <#> \a ->
                    if a.optional then "[" else ""
                      <> a.name
                      <> if a.optional then "]" else ""
  pretty (SOpt (c :| cs) pol arg) = sign <> n <> arg'
    where sign = case pol of
                  Positive -> "-"
                  Negative -> "+"
                  Both     -> "-/+" -- XXX (implemen this)
          n = fromCharArray $ A.cons c cs
          arg' = fromMaybe "" do
                  arg <#> \a ->
                    if a.optional then "[" else ""
                      <> a.name
                      <> if a.optional then "]" else ""

instance eqToken :: Eq Token where
  eq LParen            LParen             = true
  eq RParen            RParen             = true
  eq LSquare           LSquare            = true
  eq RSquare           RSquare            = true
  eq VBar              VBar               = true
  eq Colon             Colon              = true
  eq Comma             Comma              = true
  eq Dash              Dash               = true
  eq DoubleDash        DoubleDash         = true
  eq TripleDot         TripleDot          = true
  eq Newline           Newline            = true
  eq (Reference r)     (Reference r')     = r == r'
  eq (LOpt n neg arg)  (LOpt n' neg' arg')
    = (n == n') && (neg == neg') && ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              pure $ (a.name == a'.name)
                  && (a.optional == a'.optional)
            ))
  eq (SOpt (c:|cs) neg arg) (SOpt (c':|cs') neg' arg')
    = (c == c') && (cs == cs') && (neg == neg') &&
      ((isNothing arg && isNothing arg')
        || (fromMaybe false do
              a  <- arg
              a' <- arg'
              pure $ (a.name == a'.name)
                  && (a.optional == a'.optional)
            ))
  eq (AngleName n)     (AngleName n')     = n == n'
  eq (ShoutName n)     (ShoutName n')     = n == n'
  eq (Name n)          (Name n')          = n == n'
  eq (Garbage c)       (Garbage c')       = c == c'
  eq _ _                                  = false

