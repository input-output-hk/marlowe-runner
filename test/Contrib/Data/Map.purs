module Test.Contrib.Data.Map (spec) where

import Prelude

import Contrib.Data.Map (fromFoldableBy) as Map
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "fromFoldableBy" do
  it "fromFoldableBys correctly" do
    let
      actual :: Map Int { x :: Int, y :: String }
      actual = Map.fromFoldableBy _.x
        [ { x: 1, y: "huey" }
        , { x: 2, y: "dewey" }
        , { x: 3, y: "louie" }
        ]

      expected :: Map Int { x :: Int, y :: String }
      expected = Map.fromFoldable
        [ 1 /\ { x: 1, y: "huey" }
        , 2 /\ { x: 2, y: "dewey" }
        , 3 /\ { x: 3, y: "louie" }
        ]

    actual `shouldEqual` expected
