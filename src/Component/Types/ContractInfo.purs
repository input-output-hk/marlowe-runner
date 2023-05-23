module Component.Types.ContractInfo where

import Prelude

import Actus.Domain as Actus
import Contrib.Data.BigInt.PositiveBigInt (PositiveBigInt)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types as Runtime

data UserContractRole
  = ContractParty
  | ContractCounterParty
  | BothParties

derive instance Generic UserContractRole _
instance Show UserContractRole where
  show = genericShow

data ActusContractRole = ActusParty | ActusCounterParty

derive instance Generic ActusContractRole _
instance Show ActusContractRole where
  show = genericShow

-- Cash flow direction in the context of the wallet.
data UserCashFlowDirection
  = IncomingFlow
  | OutgoingFlow
  | InternalFlow

newtype CashFlowInfo = CashFlowInfo
  { -- Author of the transaction - either party or counter party.
    cashFlow :: Actus.CashFlow V1.Value V1.Party
  , sender :: ActusContractRole
  -- From the current wallet perspective (if relatd to the user).
  , userCashFlowDirection :: Maybe (UserCashFlowDirection /\ PositiveBigInt)
  , token :: V1.Token
  , transaction :: Maybe Runtime.TxHeader
  -- Value from ACTUS perspective.
  , value :: BigInt
  }

newtype MarloweInfo = MarloweInfo
  { initialContract :: V1.Contract
  , state :: Maybe V1.State
  , currentContract :: Maybe V1.Contract
  }

newtype ContractInfo = ContractInfo
  { cashFlowInfo :: Lazy (Array CashFlowInfo)
  , counterParty :: V1.Party
  , contractId :: Runtime.ContractId
  , contractTerms :: Actus.ContractTerms
  , marloweInfo :: Maybe MarloweInfo
  , endpoints ::
      { contract :: Runtime.ContractEndpoint
      , transactions :: Maybe Runtime.TransactionsEndpoint
      }
  , party :: V1.Party
  , userContractRole :: Maybe UserContractRole
  -- Use this only for debugging - all domain specific data
  -- should be precomputed and exposed as separated fields.
  , _runtime ::
      { contractHeader :: Runtime.ContractHeader
      , transactions :: Array Runtime.TxHeader
      }
  }

derive instance Newtype ContractInfo _

newtype ActusContractId = ActusContractId String

derive instance Newtype ActusContractId _
derive newtype instance Eq ActusContractId
derive newtype instance Ord ActusContractId

actusContractId :: ContractInfo -> ActusContractId
actusContractId (ContractInfo { contractTerms: Actus.ContractTerms contractTerms }) =
  ActusContractId contractTerms.contractId

actusContractType :: ContractInfo -> Actus.CT
actusContractType (ContractInfo { contractTerms: Actus.ContractTerms contractTerms }) = contractTerms.contractType

createdAt :: ContractInfo -> Maybe Runtime.BlockHeader
createdAt (ContractInfo { _runtime: { contractHeader: Runtime.ContractHeader { block } } }) = block

-- TODO: Use lazy version of `<|>` from `call-by-name`
updatedAt :: ContractInfo -> Maybe Runtime.BlockHeader
updatedAt ci@(ContractInfo { _runtime: { transactions } }) =
  do
    Runtime.TxHeader tx <- Array.last transactions
    tx.block
    <|> createdAt ci

