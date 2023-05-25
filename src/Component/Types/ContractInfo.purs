module Component.Types.ContractInfo where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Streaming (TxHeaderWithEndpoint)
import Marlowe.Runtime.Web.Types as Runtime

data UserContractRole
  = ContractParty
  | ContractCounterParty
  | BothParties

derive instance Generic UserContractRole _
instance Show UserContractRole where
  show = genericShow

-- Cash flow direction in the context of the wallet.
data UserCashFlowDirection
  = IncomingFlow
  | OutgoingFlow
  | InternalFlow

newtype MarloweInfo = MarloweInfo
  { initialContract :: V1.Contract
  , state :: Maybe V1.State
  , currentContract :: Maybe V1.Contract
  }

derive instance Eq MarloweInfo

newtype ContractInfo = ContractInfo
  { contractId :: Runtime.ContractId
  , marloweInfo :: Maybe MarloweInfo
  , endpoints ::
      { contract :: Runtime.ContractEndpoint
      , transactions :: Maybe Runtime.TransactionsEndpoint
      }
  -- Use this only for debugging - all domain specific data
  -- should be precomputed and exposed as separated fields.
  , _runtime ::
      { contractHeader :: Runtime.ContractHeader
      , transactions :: Array TxHeaderWithEndpoint

      }
  }

derive instance Newtype ContractInfo _

createdAt :: ContractInfo -> Maybe Runtime.BlockHeader
createdAt (ContractInfo { _runtime: { contractHeader: Runtime.ContractHeader { block } } }) = block

-- TODO: Use lazy version of `<|>` from `call-by-name`
updatedAt :: ContractInfo -> Maybe Runtime.BlockHeader
updatedAt ci@(ContractInfo { _runtime: { transactions } }) =
  do
    Runtime.TxHeader tx /\ _ <- Array.last transactions
    tx.block
    <|> createdAt ci
