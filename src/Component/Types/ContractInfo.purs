module Component.Types.ContractInfo where

import Prelude

import Contrib.Data.Foldable (foldMapFlipped)
import Control.Alt ((<|>))
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Validation.Semigroup (V(..))
import Effect.Aff (Aff)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, getResource') as Runtime
import Marlowe.Runtime.Web.Streaming (TxHeaderWithEndpoint)
import Marlowe.Runtime.Web.Types (BlockHeader, ContractEndpoint, ContractHeader(..), ContractId, TransactionEndpoint, TransactionsEndpoint, Tx(..), TxHeader(..)) as Runtime
import Marlowe.Runtime.Web.Types (Payout(..), ServerURL, Tags)

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
  , initialState :: V1.State
  , currencySymbol :: Maybe V1.CurrencySymbol
  , state :: Maybe V1.State
  , currentContract :: Maybe V1.Contract
  , unclaimedPayouts :: Array Payout
  }

derive instance Eq MarloweInfo

newtype ContractInfo = ContractInfo
  { contractId :: Runtime.ContractId
  , marloweInfo :: Maybe MarloweInfo
  , tags :: Tags
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
    Runtime.TxHeader tx /\ _ <- Array.head transactions
    tx.block
    <|> createdAt ci

fetchAppliedInputs :: ServerURL -> Array Runtime.TransactionEndpoint -> Aff (V (Array (Runtime.ClientError String)) (Array ((Maybe V1.InputContent) /\ V1.TimeInterval)))
fetchAppliedInputs serverURL transactionEndpoints = do
  results <- transactionEndpoints `flip parTraverse` \transactionEndpoint -> do
    Runtime.getResource' serverURL transactionEndpoint {}

  pure $ results `foldMapFlipped` case _ of
    Left err -> V (Left [err])
    Right ({ payload: { resource: Runtime.Tx { inputs, invalidBefore, invalidHereafter }}}) -> do
      let
        -- = NormalInput InputContent
        -- | MerkleizedInput InputContent String Contract
        inputToInputContent = case _ of
          V1.NormalInput inputContent -> inputContent
          V1.MerkleizedInput inputContent _ _ -> inputContent
        timeInterval = V1.TimeInterval (Instant.fromDateTime invalidBefore) (Instant.fromDateTime invalidHereafter)
      V $ Right $ case Array.uncons inputs of
        Just _ -> inputs <#> \input -> Just (inputToInputContent input) /\ timeInterval
        Nothing -> [Nothing /\ timeInterval]

