module Neodoc.Parsing.Parser where

import Prelude
import Data.Pretty
import Data.Bifunctor (rmap)
import Data.Optimize.Uncurried
import Control.Lazy (class Lazy)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt)
import Data.Either (Either(..), either)

--------------------------------------------------------------------------------
-- ParseError
--------------------------------------------------------------------------------

data ParseError e = ParseError Boolean (Either String e)

error :: ∀ e. String -> ParseError e
error msg = ParseError false (Left msg)

customError :: ∀ e. e -> ParseError e
customError e = ParseError false (Right e)

mapError :: ∀ e. (e -> e) -> ParseError e -> ParseError e
mapError f (ParseError b e) = ParseError b (rmap f e)

instance showParseError :: (Show e) => Show (ParseError e) where
  show (ParseError b e) = "(ParseError " <> show b <> " "<> show e <> ")"

instance prettyParseError :: (Pretty e) => Pretty (ParseError e) where
  pretty (ParseError false e) = either id pretty e
  pretty (ParseError true e) = "Fatal: " <> either id pretty e

-- XXX: could this be a Comonad?
extractError :: ∀ e . (String -> e) -> ParseError e -> e
extractError f (ParseError _ (Left  s)) = f s
extractError _ (ParseError _ (Right e)) = e

--------------------------------------------------------------------------------
-- ParseArgs
-- An efficient data type for feeding state to the parser
--------------------------------------------------------------------------------

data ParserArgs c s g i = ParseArgs c s g i

instance showParserArgs :: (Show c, Show s, Show g, Show i) => Show (ParserArgs c s g i) where
  show (ParseArgs c s g i) = "(ParseArgs " <> show c
                                    <> " " <> show s
                                    <> " " <> show g
                                    <> " " <> show i
                                    <> ")"

mapC :: ∀ c s g i. (c -> c) -> ParserArgs c s g i -> ParserArgs c s g i
mapC f (ParseArgs c s g i) = ParseArgs (f c) s g i
mapS :: ∀ c s g i. (s -> s) -> ParserArgs c s g i -> ParserArgs c s g i
mapS f (ParseArgs c s g i) = ParseArgs c (f s) g i
mapG :: ∀ c s g i. (g -> g) -> ParserArgs c s g i -> ParserArgs c s g i
mapG f (ParseArgs c s g i) = ParseArgs c s (f g) i
mapI :: ∀ c s g i. (i -> i) -> ParserArgs c s g i -> ParserArgs c s g i
mapI f (ParseArgs c s g i) = ParseArgs c s g (f i)

getC :: ∀ c s g i. ParserArgs c s g i -> c
getC (ParseArgs c _ _ _) = c
getS :: ∀ c s g i. ParserArgs c s g i -> s
getS (ParseArgs _ s _ _) = s
getG :: ∀ c s g i. ParserArgs c s g i -> g
getG (ParseArgs _ _ g _) = g
getI :: ∀ c s g i. ParserArgs c s g i -> i
getI (ParseArgs _ _ _ i) = i

setC :: ∀ c s g i. c -> ParserArgs c s g i -> ParserArgs c s g i
setC c = mapC (const c)
setS :: ∀ c s g i. s -> ParserArgs c s g i -> ParserArgs c s g i
setS s = mapS (const s)
setG :: ∀ c s g i. g -> ParserArgs c s g i -> ParserArgs c s g i
setG g = mapG (const g)
setI :: ∀ c s g i. i -> ParserArgs c s g i -> ParserArgs c s g i
setI i = mapI (const i)

--------------------------------------------------------------------------------
-- Step
-- Every parser yields a "Step"
--------------------------------------------------------------------------------

type IsConsumed = Boolean
type Result e a = Either (ParseError e) a
data Step e c s g i a = Step IsConsumed (ParserArgs c s g i) (Result e a)

setConsumed :: ∀ e c s g i a. Boolean -> Step e c s g i a -> Step e c s g i a
setConsumed b (Step _ a r) = Step b a r

setConsumedOr :: ∀ e c s g i a. Boolean -> Step e c s g i a -> Step e c s g i a
setConsumedOr b (Step b' a r) = Step (b || b') a r

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

data Parser e c s g i a = Parser (ParserArgs c s g i -> Step e c s g i a)

unParser :: ∀ e c s g i a. Parser e c s g i a -> ParserArgs c s g i -> Step e c s g i a
unParser (Parser f) = f

return :: ∀ e c s g i a. a -> Parser e c s g i a
return r = Parser \a -> Step false a (Right r)

instance applyParser :: Apply (Parser e c s g i) where
  apply = ap

instance applicativeParser :: Applicative (Parser e c s g i) where
  pure = return

instance functorParser :: Functor (Parser e c s g i) where
  map f p = Parser \args -> case unParser p args of
    Step b a' r -> Step b a' (f <$> r)

instance bindParser :: Bind (Parser e c s g i) where
  bind p f = Parser \args -> case unParser p args of
    Step b a' (Left  err) -> Step b a' (Left err)
    Step b a' (Right res) -> setConsumedOr b $ unParser (f res) a'

instance monadParser :: Monad (Parser e c s g i)

catch :: ∀ e c s g i a. Parser e c s g i a -> (s -> ParseError e -> Parser e c s g i a) -> Parser e c s g i a
catch p f = Parser \(args@(ParseArgs c s g i)) ->
  let step = unParser p args
   in case step of
      Step consumed (ParseArgs _ s' g' _) (Left (e@(ParseError fatal _)))
        | not (fatal || consumed) -> unParser (f s' e) (ParseArgs c s g' i)
      _ -> step

catch' :: ∀ e c s g i a. (s -> ParseError e -> Parser e c s g i a) -> Parser e c s g i a -> Parser e c s g i a
catch' = flip catch

instance altParser :: Alt (Parser e c s g i) where
  alt p1 p2 = catch p1 (\_ _ -> p2)

instance plusParser :: Plus (Parser e c s g i) where
  empty = fail "No alternative"

instance alternativeParser :: Alternative (Parser e c s g i)

instance lazyParser :: Lazy (Parser e c s g i a) where
  defer f = Parser \args -> unParser (f unit) args

instance monadZeroParser :: MonadZero (Parser e c s g i)

runParser :: ∀ e c s g i a. c -> s -> g -> i -> (Parser e c s g i a) -> Either (ParseError e) a
runParser c s g i p = case unParser p (ParseArgs c s g i) of (Step _ _ r) -> r

runParser' :: ∀ e c s g i a. Args5 c s g i (Parser e c s g i a) -> Either (ParseError e) a
runParser' (Args5 c s g i p) = case unParser p (ParseArgs c s g i) of (Step _ _ r) -> r

--------------------------------------------------------------------------------
-- Induced errors. The prime versions target the custom error type 'e'.
--------------------------------------------------------------------------------

fail :: ∀ e c s g i a. String -> Parser e c s g i a
fail message = Parser \a -> Step false a (Left $ ParseError false (Left message))

fail' :: ∀ e c s g i a. e -> Parser e c s g i a
fail' e = Parser \a -> Step false a (Left $ ParseError false (Right e))

fatal :: ∀ e c s g i a. String -> Parser e c s g i a
fatal message = Parser \a -> Step false a (Left $ ParseError true (Left message))

fatal' :: ∀ e c s g i a. e -> Parser e c s g i a
fatal' e = Parser \a -> Step false a (Left $ ParseError true (Right e))

throw :: ∀ e c s g i a. ParseError e -> Parser e c s g i a
throw e = Parser \a -> Step false a (Left e)

--------------------------------------------------------------------------------
-- Parser getters, setters, modifiers, ...
--------------------------------------------------------------------------------

getParseState :: ∀ e c s g i. Parser e c s g i (ParserArgs c s g i)
getParseState = Parser \a -> Step false a (Right a)

getConfig :: ∀ e c s g i. Parser e c s g i c
getConfig = Parser \a -> Step false a (Right (getC a))

getState :: ∀ e c s g i. Parser e c s g i s
getState = Parser \a -> Step false a (Right (getS a))

setState :: ∀ e c s g i. s -> Parser e c s g i Unit
setState s = Parser \a -> Step false (mapS (const s) a) (Right unit)

modifyState :: ∀ e c s g i. (s -> s) -> Parser e c s g i Unit
modifyState f = Parser \a -> Step false (mapS f a) (Right unit)

getGlobalState :: ∀ e c s g i. Parser e c s g i g
getGlobalState = Parser \a -> Step false a (Right (getG a))

modifyGlobalState :: ∀ e c s g i. (g -> g) -> Parser e c s g i Unit
modifyGlobalState f = Parser \a -> Step false (mapG f a) (Right unit)

setGlobalState :: ∀ e c s g i. g -> Parser e c s g i Unit
setGlobalState g = Parser \a -> Step false (mapG (const g) a) (Right unit)

getInput :: ∀ e c s g i. Parser e c s g i i
getInput = Parser \a -> Step false a (Right (getI a))

setInput :: ∀ e c s g i. i -> Parser e c s g i Unit
setInput i = Parser \a -> Step false (mapI (const i) a) (Right unit)
