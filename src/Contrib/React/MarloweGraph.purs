module Contrib.React.MarloweGraph where

import Prelude

import Component.InputHelper (ExecutionBranch(..), ExecutionPath)
import Data.Argonaut (Json, encodeJson)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Foldable (foldMap)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic (JSX, ReactComponent, element)

-- In order to use the component plus include 'reactflow/dist/style.css'

newtype ContractJson = MarloweJson Json

toContractJson :: V1.Contract -> ContractJson
toContractJson = MarloweJson <<< encodeJson

foreign import data ReactFlowInstance :: Type

foreign import _MarloweGraph
  :: ReactComponent
       { contract :: ContractJson
       , path :: Array (Nullable Int)
       , onInit :: EffectFn1 ReactFlowInstance Unit
       }

type Index = Maybe Int

branchIndex :: ExecutionBranch -> Index
branchIndex (WhenBranch idx) = idx
branchIndex (IfBranch branch) = if branch then Just 1 else Just 0

executionPathIndicies :: ExecutionPath -> Array Index
executionPathIndicies = foldMap \(_ /\ path) -> do
  let
    arr = Array.NonEmpty.toArray path <#> fst
  map branchIndex arr

type Props =
  { contract :: V1.Contract
  , executionPath :: Opt ExecutionPath
  , onInit :: Opt (ReactFlowInstance -> Effect Unit)
  }

-- { contract :: V1.Contract, path :: ExecutionPath }
marloweGraph
  :: forall props
   . NoProblem.Coerce props Props
  => props
  -> JSX
marloweGraph props = do
  let
    props' :: Props
    props' = NoProblem.coerce props

    contractJson = toContractJson props'.contract

    executionPath = props'.executionPath ! List.Nil
    executionPath' = map toNullable $ executionPathIndicies executionPath

    onInit = case NoProblem.toMaybe props'.onInit of
      Just fn -> mkEffectFn1 fn
      Nothing -> mkEffectFn1 \_ -> pure unit

  element _MarloweGraph { contract: contractJson, path: executionPath', onInit }
