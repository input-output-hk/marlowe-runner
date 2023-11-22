module Component.Types.ContractInfoMap
  ( ContractInfoMap(..)
  , insertContractCreated
  , insertContractUpdated
  , updateSynced
  , getContractsMap
  , uninitialized
  ) where

import Prelude

import Component.Types (ContractInfo(..))
import Component.Types.ContractInfo (ContractCreated, ContractUpdated(..), MarloweInfo(..), NotSyncedYet(..), SomeContractInfo(..), ContractStatus(..), addContractCreated, addContractUpdated, emptyNotSyncedYet, someContractInfoFromContractCreated, someContractInfoFromContractUpdated)
import Component.Types.ContractInfo as ContractInfo
import Contrib.Cardano (Slotting, slotToTimestamp)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.These (These, maybeThese, theseLeft, theseRight)
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics (emptyState) as V1
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsMap)
import Marlowe.Runtime.Web.Types (ContractHeader(..), PolicyId(..), TxHeader(..), TxOutRef(..))
import Marlowe.Runtime.Web.Types as Runtime

--  * The `contractsMap` contains correct final view of the contracts and it is a public piece of the API
--    The `contractsSources` piece is private.
--  * The `contractsMap` creation depends on the union ordering so we have to keep only
--    the smaller `NotSyncedYet` `Map`s up to date in `contractsSources`.
--  * The `ContractWithTransactionsMap` contains all the contracts which are/were synced meaning
--    it could contain duplicates for some contracts from `NotSyncedYet` `Map`s.
data ContractInfoMap
  = UninitializedContractInfoMap
      { slotting :: Slotting
      }
  | ContractInfoMap
      { slotting :: Slotting
      , contractsSources :: These NotSyncedYet ContractWithTransactionsMap
      , contractsMap :: Map Runtime.ContractId SomeContractInfo
      }

uninitialized :: Slotting -> ContractInfoMap
uninitialized slotting = UninitializedContractInfoMap { slotting }

-- This is private constructor which doesn't check for consistency
-- all the modification should be proxied through exposed methods.
--
-- TODO: We can optimize this by expecting `Map ContractId ContractInfo` instead
-- of `ContractWithTransactionsMap` so we don't reconstruct the map every time.
mkContractInfoMap :: Slotting -> Maybe ContractWithTransactionsMap -> Maybe NotSyncedYet -> ContractInfoMap
mkContractInfoMap slotting possiblySynced possiblyNotSyncedYet = fromMaybe (UninitializedContractInfoMap { slotting }) do
  contractsSources <- maybeThese possiblyNotSyncedYet possiblySynced
  let
    ns = fromMaybe Map.empty do
      NotSyncedYet { created, updated } <- possiblyNotSyncedYet
      pure $ map someContractInfoFromContractCreated created
        `Map.union` map someContractInfoFromContractUpdated updated
    s = fromMaybe Map.empty do
      synced <- possiblySynced
      pure $ Map.catMaybes $ synced <#> \contractWithTransactions -> do
        let
          { contract:
              { resource: contractHeader@(Runtime.ContractHeader { roleTokenMintingPolicyId, tags, status: createTxStatus })
              , links: endpoints
              }
          , contractState
          , transactions: possibleTransactions
          } = contractWithTransactions
          possibleMarloweInfo = do
            Runtime.ContractState contractState' <- contractState
            pure $ MarloweInfo
              { initialContract: contractState'.initialContract
              , currencySymbol: case roleTokenMintingPolicyId of
                  PolicyId "" -> Nothing
                  PolicyId policyId -> Just $ policyId
              , state: contractState'.state
              , currentContract: contractState'.currentContract
              , initialState: V1.emptyState -- FIXME: No initial state on the API LEVEL?
              , unclaimedPayouts: contractState'.unclaimedPayouts
              }
          Runtime.ContractHeader { contractId, block } = contractHeader
          blockSlotTimestamp (Runtime.BlockHeader { slotNo }) = slotToTimestamp slotting slotNo

          contractStatus = fromMaybe (StillFetching { possibleMarloweInfo, endpoint: endpoints.contract }) do
            marloweInfo <- possibleMarloweInfo
            transactionsHeadersWithEndpoints <- possibleTransactions
            transactionsEndpoint <- endpoints.transactions
            let
              txStatuses = transactionsHeadersWithEndpoints <#> \(TxHeader { status: txStatus } /\ _) -> txStatus
              endpoints' = { contract: endpoints.contract, transactions: transactionsEndpoint }
            pure $ case txStatuses, createTxStatus of
              [], Runtime.Confirmed -> Confirmed
                { marloweInfo, endpoints: endpoints', transactionsHeadersWithEndpoints }
              [], _ -> NotConfirmedCreation
                { marloweInfo, endpoint: endpoints.contract, txStatus: createTxStatus }
              _, _ ->
                case Array.find (_ /= Runtime.Confirmed) txStatuses of
                  Just txStatus -> NotConfirmedInputsApplication
                    { marloweInfo
                    , endpoints: endpoints'
                    , transactionsHeadersWithEndpoints
                    , txStatus
                    }
                  Nothing -> Confirmed
                    { marloweInfo
                    , endpoints: endpoints'
                    , transactionsHeadersWithEndpoints
                    }

          createdAt :: Maybe Instant
          createdAt = blockSlotTimestamp <$> block

          updatedAt :: Maybe Instant
          updatedAt = do
            transactions <- possibleTransactions
            Runtime.TxHeader tx /\ _ <- Array.head transactions
            blockSlotTimestamp <$> tx.block

        pure $ SyncedConractInfo $ ContractInfo $
          { contractId
          , createdAt
          , updatedAt
          , contractStatus
          , tags
          , _runtime: { contractHeader, transactions: possibleTransactions }
          }
  pure $ ContractInfoMap { contractsMap: ns `Map.union` s, contractsSources, slotting }

contractInfoMapSlotting :: ContractInfoMap -> Slotting
contractInfoMapSlotting = case _ of
  UninitializedContractInfoMap { slotting } -> slotting
  ContractInfoMap { slotting } -> slotting

getContractsMap :: ContractInfoMap -> Maybe (Map Runtime.ContractId SomeContractInfo)
getContractsMap (UninitializedContractInfoMap _) = Nothing
getContractsMap (ContractInfoMap { contractsMap }) = Just contractsMap

-- * We want to rely on server data as much as we can.
-- * `ContractUpdated` doesn't store the tx details - it is only a placeholder which
--   tells us that the contract was updated and we await for the server.
-- * As quickly as we get new data (either extra txId or changed txId list) we want do
--   ignore the old `ContractUpdated` and use the new one.
isContractUpdateStillRelevant :: ContractUpdated -> ContractWithTransactionsMap -> Boolean
isContractUpdateStillRelevant contractUpdated contractsMap = do
  let
    ContractUpdated { contractInfo } = contractUpdated
    ContractInfo { contractId, _runtime: contractUpdatedTxsInfo } = contractInfo
  case Map.lookup contractId contractsMap of
    -- If contract is missing we drop the update - probably was removed
    Nothing -> false
    -- If contract is not synced (impossible case) then we keep the update
    Just { transactions: Nothing } -> true
    Just { contract: { resource: contractHeader }, transactions } -> do
      let
        contractMapTxsInfo = { contractHeader, transactions }
        txIds { contractHeader: ContractHeader { contractId: TxOutRef { txId } }, transactions: txs } = do
          let
            applyInputsTxIds Nothing = []
            applyInputsTxIds (Just txHeaders) = txHeaders <#> \(TxHeader { transactionId } /\ _) -> transactionId
          txId Array.: (applyInputsTxIds txs)
      (txIds contractMapTxsInfo) == (txIds contractUpdatedTxsInfo)

updateSynced :: ContractWithTransactionsMap -> ContractInfoMap -> ContractInfoMap
updateSynced synced contractInfoMap = do
  let
    possiblyNotSyncedYet = case contractInfoMap of
      UninitializedContractInfoMap _ -> Nothing
      ContractInfoMap { contractsSources } -> theseLeft contractsSources
    slotting = contractInfoMapSlotting contractInfoMap

    syncedKeys = Map.keys synced
    possiblyNotSyncedYet' = do
      NotSyncedYet { created, updated } <- possiblyNotSyncedYet
      let
        updated' = Map.filter (_ `isContractUpdateStillRelevant` synced) updated

      pure $ NotSyncedYet $
        { created: Map.filter (\(ContractInfo.ContractCreated { contractId }) -> contractId `not Set.member` syncedKeys) created
        , updated: updated'
        }
  mkContractInfoMap slotting (Just synced) possiblyNotSyncedYet'

insertContractCreated :: ContractCreated -> ContractInfoMap -> ContractInfoMap
insertContractCreated contractCreated@(ContractInfo.ContractCreated { contractId }) contractInfoMap = do
  let
    possiblySynced = case contractInfoMap of
      UninitializedContractInfoMap _ -> Nothing
      ContractInfoMap { contractsSources } -> theseRight contractsSources

    isSyncedAlready = case possiblySynced of
      Nothing -> false
      Just synced -> contractId `Map.member` synced

    add =
      if isSyncedAlready then identity
      else addContractCreated contractCreated

    notSyncedYet = add $ case contractInfoMap of
      UninitializedContractInfoMap _ -> emptyNotSyncedYet
      ContractInfoMap { contractsSources } ->
        fromMaybe emptyNotSyncedYet $ theseLeft contractsSources
    slotting = contractInfoMapSlotting contractInfoMap
  mkContractInfoMap slotting possiblySynced $ Just notSyncedYet

insertContractUpdated :: ContractUpdated -> ContractInfoMap -> ContractInfoMap
insertContractUpdated contractUpdated contractInfoMap = do
  let
    possiblySynced = case contractInfoMap of
      UninitializedContractInfoMap _ -> Nothing
      ContractInfoMap { contractsSources } -> theseRight contractsSources

  if maybe false (isContractUpdateStillRelevant contractUpdated) possiblySynced then do
    let
      notSyncedYet = addContractUpdated contractUpdated $ case contractInfoMap of
        UninitializedContractInfoMap _ -> emptyNotSyncedYet
        ContractInfoMap { contractsSources } ->
          fromMaybe emptyNotSyncedYet $ theseLeft contractsSources
      slotting = contractInfoMapSlotting contractInfoMap
    mkContractInfoMap slotting possiblySynced $ Just notSyncedYet
  else
    contractInfoMap
