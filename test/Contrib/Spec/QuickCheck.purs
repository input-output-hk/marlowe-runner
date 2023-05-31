module Test.Contrib.Spec.QuickCheck where

import Prelude

import Effect.Class (liftEffect)
import Test.QuickCheck (class Testable, quickCheck)
import Test.Spec (Spec, it, itOnly, pending')

prop :: forall prop. Testable prop => String -> prop -> Spec Unit
prop name = it name <<< liftEffect <<< quickCheck

propOnly :: forall prop. Testable prop => String -> prop -> Spec Unit
propOnly name = itOnly name <<< liftEffect <<< quickCheck

propPending :: forall prop. Testable prop => String -> prop -> Spec Unit
propPending name = pending' name <<< liftEffect <<< quickCheck
