module Test.Spec.ForeignSpec
-- (foreignSpec)
where

-- import Prelude

-- import Control.Alt ((<|>))
-- import Control.Apply ((*>), (<*))
-- import Control.Monad.Except (runExcept)
-- import Data.Array as A
-- import Data.Array.Partial as AU
-- import Data.Either (Either(..), either, fromLeft, fromRight)
-- import Data.Foldable (any)
-- import Data.Foldable (intercalate, for_)
-- import Data.Function.Uncurried
-- import Data.List (List(..), (:), many, toUnfoldable)
-- import Data.List.NonEmpty as NEL
-- import Data.Maybe (Maybe(..), fromMaybe, fromJust)
-- import Data.Newtype (unwrap)
-- import Data.Pretty (pretty)
-- import Data.String as String
-- import Data.Tuple (Tuple(..), fst, snd)
-- import Debug.Trace
-- import Effect (Effect())
-- import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Exception (error, throwException)
-- import Partial.Unsafe
-- import Partial.Unsafe (unsafePartial)
-- import Test.Spec (Spec(), describe, it)
-- import Test.Support (vliftEff)
-- import Text.Wrap (dedent)
-- import Unsafe.Coerce (unsafeCoerce)

-- import Neodoc as Neodoc
-- import Neodoc.SpecConversions (fromEmptyableSpec)
-- import Neodoc.Data.UsageLayout

-- import Test.Support.CompatParser


-- foreignSpec :: List Test -> Spec Effect Unit
-- foreignSpec tests = describe "Crossing JS/purescript" do
--   for_ tests \(Test { doc, kases }) -> do
--     if any (\(Kase k) -> case k.out of
--       Left e | e == "spec-error" -> true -- skip
--       Left  _ -> false
--       Right _ -> false
--     ) kases
--       then pure unit
--       else
--         it doc do

--           -- XXX: Manually break the execution context in order to avoid to
--           --      avoid stack overflows by executing a large amount of Aff
--           --      actions that run purely synchronous. Ideally, we would run
--           --      the `Aff` action using it's `MonadRec` instance.
--           -- Refer: https://github.com/owickstrom/purescript-spec/issues/24
--           -- later (pure unit)

--           vliftEff do
--             let helpText = dedent doc

--             -- parse the spec as a purescript value
--             expected <- either (throwException <<< error <<< pretty) pure do
--               Neodoc.parseHelpText helpText

--             -- parse the spec as a JS value
--             -- we could use `specToForeign` on the ouput above, but this -
--             -- albeit slower - gives more confidence that everything's
--             -- alright.
--             input <- unsafeCoerce do
--               runFn1 Neodoc.parseHelpTextJS helpText

--             let result = fromEmptyableSpec <$> F.read input

--             case runExcept result of
--               Left es ->
--                 let e = NEL.head es
--                  in throwException $ error $ F.prettyForeignError e
--               Right output -> do
--                 if output /= expected
--                   -- XXX: Would be cool to get some sort of diffing in here.
--                   then throwException $ error $
--                     "input and output mismatches:\n"
--                       <> "Expected:"
--                       <> "\n\n"
--                       <> pretty expected
--                       <> "\n\n"
--                       <> "Received:"
--                       <> "\n\n"
--                       <> pretty output
--                   else pure unit
