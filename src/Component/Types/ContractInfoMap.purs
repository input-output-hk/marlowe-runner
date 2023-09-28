module Component.Types.ContractInfoMap
  ( ContractInfoMap
  , insertContractCreated
  , insertContractUpdated
  , updateSynced
  , getContractsMap
  , uninitialized
  ) where

import Prelude

import Component.Types (ContractInfo(..))
import Component.Types.ContractInfo (ContractCreated, ContractUpdated(..), MarloweInfo(..), NotSyncedYet(..), SomeContractInfo(..), addContractCreated, addContractUpdated, emptyNotSyncedYet, someContractInfoFromContractCreated, someContractInfoFromContractUpdated)
import Component.Types.ContractInfo as ContractInfo
import Contrib.Cardano (Slotting, slotToTimestamp)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.These (These, maybeThese, theseLeft, theseRight)
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics (emptyState) as V1
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsMap)
import Marlowe.Runtime.Web.Types (ContractHeader(..), PolicyId(..), TxHeader(..), TxOutRef(..))
import Marlowe.Runtime.Web.Types as Runtime

--  * The `contractsMap` contains correct final view of the contracts and is a public piece of the API
--    the `contractsSources` piece is private.
--  * The `contractsMap` creation depends on the union ordering so we have to keep only
--    the smaller `NotSyncedYet` `Map`s up to date in `contractsSources`.
--  * The `ContractWithTransactionsMap` contains all the contracts which are synced meaning
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
              { resource: contractHeader@(Runtime.ContractHeader { roleTokenMintingPolicyId, tags })
              , links: endpoints
              }
          , contractState
          , transactions
          } = contractWithTransactions
          marloweInfo = do
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

          createdAt :: Maybe Instant
          createdAt = blockSlotTimestamp <$> block

          updatedAt :: Maybe Instant
          updatedAt = do
            Runtime.TxHeader tx /\ _ <- Array.head transactions
            blockSlotTimestamp <$> tx.block

        pure $ SyncedConractInfo $ ContractInfo $
          { contractId
          , createdAt
          , updatedAt
          , endpoints
          , marloweInfo
          , tags
          , _runtime: { contractHeader, transactions }
          }
  pure $ ContractInfoMap { contractsMap: ns `Map.union` s, contractsSources, slotting }

contractInfoMapSlotting :: ContractInfoMap -> Slotting
contractInfoMapSlotting = case _ of
  UninitializedContractInfoMap { slotting } -> slotting
  ContractInfoMap { slotting } -> slotting

getContractsMap :: ContractInfoMap -> Maybe (Map Runtime.ContractId SomeContractInfo)
getContractsMap (UninitializedContractInfoMap _) = Nothing
getContractsMap (ContractInfoMap { contractsMap }) = Just contractsMap

-- We want to check if the contract in the synced map is exactly the same as the contract
-- which are updating - the safest way to do equality check is to use transaction ids.
--
-- If the contract was changed or is missing we have to ignore the update information - it is no longer relevant.
-- If it is we want to remove the contract from the synced map and add it to the not synced yet map.
--
-- This check is rather a coner case checking (rarely happens on the app level - too short timespan)
-- but still valid from the structure consistency perspective.
isContractUpdateStillRelevant :: ContractUpdated -> Maybe ContractWithTransactionsMap -> Boolean
isContractUpdateStillRelevant contractUpdated possibleContractsMap = fromMaybe false do
  let
    ContractUpdated { contractInfo } = contractUpdated
    ContractInfo { contractId, _runtime: runtime } = contractInfo

  contractsMap <- possibleContractsMap
  contractWithTransactions <- Map.lookup contractId contractsMap
  let
    runtime' = do
      let
        { contract: { resource: contractHeader }, transactions } = contractWithTransactions
      { contractHeader, transactions }
    allTxIds { contractHeader: ContractHeader { contractId: TxOutRef { txId } }, transactions } =
      txId Array.: (transactions <#> \(TxHeader { transactionId } /\ _) -> transactionId)
    txIds = allTxIds runtime
    txIds' = allTxIds runtime'
  pure $ txIds == txIds'

updateSynced :: Maybe ContractWithTransactionsMap -> ContractInfoMap -> ContractInfoMap
updateSynced possiblySynced contractInfoMap = do
  let
    possiblyNotSyncedYet = case contractInfoMap of
      UninitializedContractInfoMap _ -> Nothing
      ContractInfoMap { contractsSources } -> theseLeft contractsSources
    slotting = contractInfoMapSlotting contractInfoMap

    syncedKeys = fromMaybe mempty do
      synced <- possiblySynced
      pure $ Map.keys synced

    possiblyNotSyncedYet' = do
      NotSyncedYet { created, updated } <- possiblyNotSyncedYet
      let
        updated' = Map.filter (_ `isContractUpdateStillRelevant` possiblySynced) updated

      pure $ NotSyncedYet $
        { created: Map.filter (\(ContractInfo.ContractCreated { contractId }) -> contractId `not Set.member` syncedKeys) created
        , updated: updated'
        }
  mkContractInfoMap slotting possiblySynced possiblyNotSyncedYet'

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

  if isContractUpdateStillRelevant contractUpdated possiblySynced then do
    let
      notSyncedYet = addContractUpdated contractUpdated $ case contractInfoMap of
        UninitializedContractInfoMap _ -> emptyNotSyncedYet
        ContractInfoMap { contractsSources } ->
          fromMaybe emptyNotSyncedYet $ theseLeft contractsSources
      slotting = contractInfoMapSlotting contractInfoMap
    mkContractInfoMap slotting possiblySynced $ Just notSyncedYet
  else
    contractInfoMap
