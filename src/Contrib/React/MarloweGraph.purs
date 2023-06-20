module Contrib.React.MarloweGraph where

import Prelude

import Data.Argonaut (Json, encodeJson)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic (JSX, ReactComponent, element)
import Record as Record
import Type.Prelude (Proxy(..))

-- In order to use the component plus include 'reactflow/dist/style.css'

newtype ContractJson = MarloweJson Json

toContractJson :: V1.Contract -> ContractJson
toContractJson = MarloweJson <<< encodeJson

foreign import _MarloweGraph :: ReactComponent { contract :: ContractJson }

marloweGraph :: { contract :: V1.Contract } -> JSX
marloweGraph r@{ contract } = do
  let
    contractJson = toContractJson contract
  element _MarloweGraph $ Record.set (Proxy @"contract") contractJson r
