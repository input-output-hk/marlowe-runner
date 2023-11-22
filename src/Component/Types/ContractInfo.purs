module Component.Types.ContractInfo where

import Prelude

import Contrib.Data.Foldable (foldMapFlipped)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Validation.Semigroup (V(..))
import Effect.Aff (Aff)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, getResource') as Runtime
import Marlowe.Runtime.Web.Streaming (TxHeaderWithEndpoint)
import Marlowe.Runtime.Web.Streaming (TxHeaderWithEndpoint) as Runtime
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader, ContractId, TransactionEndpoint, TransactionsEndpoint, Tx(..), TxStatus) as Runtime
import Marlowe.Runtime.Web.Types (Payout, ServerURL, Tags)

newtype MarloweInfo = MarloweInfo
  { initialContract :: V1.Contract
  , initialState :: V1.State
  , currencySymbol :: Maybe V1.CurrencySymbol
  , state :: Maybe V1.State
  , currentContract :: Maybe V1.Contract
  , unclaimedPayouts :: Array Payout
  }

derive instance Eq MarloweInfo
derive instance Generic MarloweInfo _

-- So currently we rely on Runtime that at some point
-- unconfirmed transactions will be confirmed or rejected.
--
-- If we should not rely on the Runtime we should keep the submission
-- time and the transaction itself so we can reject the transaction based
-- on some timeout or the tx validity interval.
data ContractStatus
  = StillFetching
      { possibleMarloweInfo :: Maybe MarloweInfo
      , endpoint :: Runtime.ContractEndpoint
      }
  | Confirmed
      { marloweInfo :: MarloweInfo
      , transactionsHeadersWithEndpoints ::
          Array Runtime.TxHeaderWithEndpoint
      , endpoints ::
          { contract :: Runtime.ContractEndpoint
          , transactions :: Runtime.TransactionsEndpoint
          }
      }
  | NotConfirmedCreation
      { marloweInfo :: MarloweInfo
      , endpoint :: Runtime.ContractEndpoint
      , txStatus :: Runtime.TxStatus
      }
  | NotConfirmedInputsApplication
      { marloweInfo :: MarloweInfo
      , transactionsHeadersWithEndpoints ::
          Array Runtime.TxHeaderWithEndpoint
      , txStatus :: Runtime.TxStatus
      , endpoints ::
          { contract :: Runtime.ContractEndpoint
          , transactions :: Runtime.TransactionsEndpoint
          }
      }

derive instance Eq ContractStatus
derive instance Generic ContractStatus _

contractStatusMarloweInfo :: ContractStatus -> Maybe MarloweInfo
contractStatusMarloweInfo = case _ of
  StillFetching { possibleMarloweInfo } -> possibleMarloweInfo
  Confirmed { marloweInfo } -> Just marloweInfo
  NotConfirmedCreation { marloweInfo } -> Just marloweInfo
  NotConfirmedInputsApplication { marloweInfo } -> Just marloweInfo

contractStatusTransactionsHeadersWithEndpoints :: ContractStatus -> Maybe (Array Runtime.TxHeaderWithEndpoint)
contractStatusTransactionsHeadersWithEndpoints = case _ of
  StillFetching {} -> Nothing
  Confirmed { transactionsHeadersWithEndpoints } -> Just transactionsHeadersWithEndpoints
  NotConfirmedCreation {} -> Nothing
  NotConfirmedInputsApplication { transactionsHeadersWithEndpoints } -> Just transactionsHeadersWithEndpoints

contractStatusTransactionsEndpoint :: ContractStatus -> Maybe Runtime.TransactionsEndpoint
contractStatusTransactionsEndpoint = case _ of
  StillFetching {} -> Nothing
  Confirmed { endpoints: { transactions } } -> Just transactions
  NotConfirmedInputsApplication { endpoints: { transactions } } -> Just transactions
  NotConfirmedCreation {} -> Nothing

newtype ContractInfo = ContractInfo
  { contractId :: Runtime.ContractId
  , contractStatus :: ContractStatus
  , tags :: Tags
  , createdAt :: Maybe Instant
  , updatedAt :: Maybe Instant
  -- Use this only for debugging - all domain specific data
  -- should be precomputed and exposed as separate fields.
  , _runtime ::
      { contractHeader :: Runtime.ContractHeader
      , transactions :: Maybe (Array TxHeaderWithEndpoint)
      }
  }

derive instance Eq ContractInfo
derive instance Newtype ContractInfo _

fetchAppliedInputs :: ServerURL -> Array Runtime.TransactionEndpoint -> Aff (V (Array (Runtime.ClientError String)) (Array ((Maybe V1.InputContent) /\ V1.TimeInterval)))
fetchAppliedInputs serverURL transactionEndpoints = do
  results <- transactionEndpoints `flip parTraverse` \transactionEndpoint -> do
    Runtime.getResource' serverURL transactionEndpoint {} {}

  pure $ results `foldMapFlipped` case _ of
    Left err -> V (Left [ err ])
    Right ({ payload: { resource: Runtime.Tx { inputs, invalidBefore, invalidHereafter } } }) -> do
      let
        inputToInputContent = case _ of
          V1.NormalInput inputContent -> inputContent
          V1.MerkleizedInput inputContent _ _ -> inputContent
        timeInterval = V1.TimeInterval (Instant.fromDateTime invalidBefore) (Instant.fromDateTime invalidHereafter)
      V $ Right $ case Array.uncons inputs of
        Just _ -> inputs <#> \input -> Just (inputToInputContent input) /\ timeInterval
        Nothing -> [ Nothing /\ timeInterval ]

-- After we submit of the tx we have to wait till streaming thread catches up
-- so it is better to keep the information also in the state and display new
-- contracts as they go.
type ContractCreatedDetails =
  { contract :: V1.Contract
  -- , initialState :: V1.State
  , submittedAt :: Instant
  , tags :: Tags
  , contractEndpoint :: Runtime.ContractEndpoint
  , contractId :: Runtime.ContractId
  }

newtype ContractCreated = ContractCreated ContractCreatedDetails

derive instance Newtype ContractCreated _

type ContractUpdatedDetails =
  { contractInfo :: ContractInfo
  , transactionInput :: V1.TransactionInput
  , outputContract :: V1.Contract
  , outputState :: V1.State
  , submittedAt :: Instant
  }

newtype ContractUpdated = ContractUpdated ContractUpdatedDetails

derive instance Newtype ContractUpdated _

newtype NotSyncedYet = NotSyncedYet
  { created :: Map Runtime.ContractId ContractCreated
  , updated :: Map Runtime.ContractId ContractUpdated
  }

emptyNotSyncedYet :: NotSyncedYet
emptyNotSyncedYet = NotSyncedYet
  { created: Map.empty, updated: Map.empty }

addContractCreated :: ContractCreated -> NotSyncedYet -> NotSyncedYet
addContractCreated cc@(ContractCreated { contractId }) (NotSyncedYet props) = NotSyncedYet do
  props { created = Map.insert contractId cc props.created }

addContractUpdated :: ContractUpdated -> NotSyncedYet -> NotSyncedYet
addContractUpdated cu@(ContractUpdated { contractInfo }) (NotSyncedYet props) = NotSyncedYet do
  let
    ContractInfo { contractId } = contractInfo
  props { updated = Map.insert contractId cu props.updated }

-- FIXME: We should change naming scheme because it can be missleading:
-- * the ContractInfo internally can be unsynced.
-- * `NotSynced*` really means that it was submitted but not refetched by the streaming thread.
--
-- We want to have unified interface for both synced and not synced contracts
-- so we can display them in the same way etc.
data SomeContractInfo
  = SyncedConractInfo ContractInfo
  | NotSyncedCreatedContract ContractCreatedDetails
  | NotSyncedUpdatedContract ContractUpdatedDetails

derive instance Eq SomeContractInfo

data SomeContractInfoStatus
  = ContractStatus ContractStatus
  | Creating
  | Updating

derive instance Eq SomeContractInfoStatus
derive instance Generic SomeContractInfoStatus _

someContractInfoStatus :: SomeContractInfo -> SomeContractInfoStatus
someContractInfoStatus (SyncedConractInfo (ContractInfo { contractStatus })) = ContractStatus contractStatus
someContractInfoStatus (NotSyncedCreatedContract {}) = Creating
someContractInfoStatus (NotSyncedUpdatedContract {}) = Updating

someContractInfoContractId :: SomeContractInfo -> Runtime.ContractId
someContractInfoContractId (SyncedConractInfo (ContractInfo { contractId })) = contractId
someContractInfoContractId (NotSyncedCreatedContract { contractId }) = contractId
someContractInfoContractId (NotSyncedUpdatedContract { contractInfo: ContractInfo { contractId } }) = contractId

someContractInfoFromContractInfo :: ContractInfo -> SomeContractInfo
someContractInfoFromContractInfo = SyncedConractInfo

someContractInfoFromContractCreated :: ContractCreated -> SomeContractInfo
someContractInfoFromContractCreated (ContractCreated details) = NotSyncedCreatedContract details

someContractInfoFromContractUpdated :: ContractUpdated -> SomeContractInfo
someContractInfoFromContractUpdated (ContractUpdated details) = NotSyncedUpdatedContract details

createdAt :: SomeContractInfo -> Maybe Instant
createdAt (SyncedConractInfo (ContractInfo { createdAt: c })) = c
createdAt (NotSyncedCreatedContract { submittedAt: c }) = Just c
createdAt (NotSyncedUpdatedContract { contractInfo: ContractInfo { createdAt: c } }) = c

updatedAt :: SomeContractInfo -> Maybe Instant
updatedAt (SyncedConractInfo (ContractInfo { updatedAt: u })) = u
updatedAt (NotSyncedCreatedContract { submittedAt: u }) = Just u
updatedAt (NotSyncedUpdatedContract { submittedAt: u }) = Just u

someContractTags :: SomeContractInfo -> Tags
someContractTags (SyncedConractInfo (ContractInfo { tags: t })) = t
someContractTags (NotSyncedCreatedContract { tags: t }) = t
someContractTags (NotSyncedUpdatedContract { contractInfo: ContractInfo { tags: t } }) = t

someContractContractId :: SomeContractInfo -> Runtime.ContractId
someContractContractId (SyncedConractInfo (ContractInfo { contractId: c })) = c
someContractContractId (NotSyncedCreatedContract { contractId: c }) = c
someContractContractId (NotSyncedUpdatedContract { contractInfo: ContractInfo { contractId: c } }) = c

someContractCurrentContract :: SomeContractInfo -> Maybe V1.Contract
someContractCurrentContract (SyncedConractInfo (ContractInfo { contractStatus })) = do
  MarloweInfo { currentContract } <- contractStatusMarloweInfo contractStatus
  currentContract
someContractCurrentContract (NotSyncedCreatedContract { contract: c }) = Just c
someContractCurrentContract (NotSyncedUpdatedContract { outputContract: c }) = Just c
