module Contrib.React.MarloweGraph where

import Prelude

import Component.InputHelper (ExecutionBranch(..), ExecutionPath, StartPathSelection(..))
import Data.Argonaut (Json, encodeJson)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

-- In order to use the component plus include 'reactflow/dist/style.css'

newtype ContractJson = MarloweJson Json

toContractJson :: V1.Contract -> ContractJson
toContractJson = MarloweJson <<< encodeJson

foreign import data ReactFlowInstance :: Type

foreign import data GraphExecutionPath :: Type

stillPossible :: GraphExecutionPath
stillPossible = unsafeCoerce "still-possible"

skipped :: GraphExecutionPath
skipped = unsafeCoerce "skipped"

foreign import data GraphPathItem :: Type

idxToGraphPathItem :: Index -> GraphPathItem
idxToGraphPathItem (Index i) = unsafeCoerce $ toNullable i
idxToGraphPathItem StartSelection = unsafeCoerce "start-selection"

pathIndices :: Array GraphPathItem -> GraphExecutionPath
pathIndices = unsafeCoerce

executionPathIndicies :: ExecutionPath -> Array Index
executionPathIndicies = foldMap \((_ /\ _ /\ StartPathSelection startPathSelection) /\ path) -> do
  let
    arr = Array.NonEmpty.toArray path <#> fst
    idxes = (map branchIndex arr)
  if startPathSelection then Array.cons StartSelection idxes
  else idxes

foreign import _MarloweGraph
  :: ReactComponent
       { contract :: ContractJson
       , path :: GraphExecutionPath
       , onInit :: EffectFn1 ReactFlowInstance Unit
       }

data Index
  = Index (Maybe Int)
  | StartSelection

branchIndex :: ExecutionBranch -> Index
branchIndex (WhenBranch idx) = Index idx
branchIndex (IfBranch branch) = Index $ if branch then Just 1 else Just 0

type Props =
  { contract :: V1.Contract
  , executionPath :: Opt ExecutionPath
  , onInit :: Opt (ReactFlowInstance -> Effect Unit)
  }

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

    graphExecutionPath = case NoProblem.toMaybe props'.executionPath of
      Nothing -> stillPossible
      Just p -> pathIndices $ map idxToGraphPathItem $ executionPathIndicies p

    onInit = case NoProblem.toMaybe props'.onInit of
      Just fn -> mkEffectFn1 fn
      Nothing -> mkEffectFn1 \_ -> pure unit

  element _MarloweGraph { contract: contractJson, path: graphExecutionPath, onInit }
