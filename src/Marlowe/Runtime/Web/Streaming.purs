module Marlowe.Runtime.Web.Streaming
  ( contracts
  , contractsTransactions
  , contractsStates
  , contractsWithTransactions
  , mkContractsWithTransactions
  , ContractEvent
  , ContractMap
  , ContractStream(..)
  , ContractStateStream(..)
  , ContractStateEvent(..)
  , ContractStateMap(..)
  , ContractTransactionsEvent
  , ContractTransactionsMap
  , ContractTransactionsStream(..)
  , ContractWithTransactionsEvent(..)
  , ContractWithTransactionsMap
  , ContractWithTransactions
  , ContractWithTransactionsStream(..)
  , MaxPages(..)
  , PollingInterval(..)
  , RequestInterval(..)
  , TxHeaderWithEndpoint(..)
  ) where

import Prelude

import Contrib.Data.Map (New(..), Old(..), additions, deletions, fromFoldableBy, updates) as Map
import Contrib.Effect as Effect
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class (forever)
import Control.Parallel (parSequence)
import Data.Filterable (filter)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map (catMaybes, empty, filter, fromFoldable, lookup, union) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype as Newtype
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds, delay)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription (Listener)
import Halogen.Subscription as Subscription
import Marlowe.Runtime.Web.Client (foldMapMContractPages, getPages', getResource')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractId, ContractState, GetContractResponse, GetContractsResponse, ServerURL, TransactionEndpoint, TransactionsEndpoint, TxHeader, api)

-- | API CAUTION: We update the state in chunks but send the events one by one. This means that
-- | the event handler can see some state changes (in `getLiveState`) before it receives some notifications.
-- | `getState` provides a consistent but possibly blocking view of the state.

data ContractEvent
  = Addition GetContractsResponse
  | Deletion GetContractsResponse
  | Update { old :: GetContractsResponse, new :: GetContractsResponse }

contractsById :: Array GetContractsResponse -> Map ContractId GetContractsResponse
contractsById = Map.fromFoldableBy $ _.contractId <<< Newtype.unwrap <<< _.resource

newtype RequestInterval = RequestInterval Milliseconds

newtype PollingInterval = PollingInterval Milliseconds

-- | TODO: Provide nicer types.
type ContractMap = Map ContractId GetContractsResponse

newtype ContractStream = ContractStream
  { emitter :: Subscription.Emitter ContractEvent
  , getLiveState :: Effect ContractMap
  , getState :: Aff ContractMap
  , start :: Aff Unit
  }

newtype MaxPages = MaxPages Int

-- | FIXME: take closer at error handling woudn't this component break in the case of network error?
-- | TODO: we should return `Aff` or fiber and allow more flexible "threading" management.
-- Use constraint at the end: `Warn (Text "pushPullContractsStreams is deprecated, use web socket based implementation instead!")`
contracts
  :: PollingInterval
  -> RequestInterval
  -> (GetContractsResponse -> Boolean)
  -> Maybe MaxPages
  -> ServerURL
  -> Aff ContractStream
contracts (PollingInterval pollingInterval) (RequestInterval requestInterval) filterContracts possibleMaxPages serverUrl = do
  contractsRef <- liftEffect $ Ref.new Map.empty
  pageNumberRef <- liftEffect $ Ref.new 0
  contractsAVar <- AVar.empty

  { emitter, listener } <- liftEffect Subscription.create

  let
    range = Nothing

  let
    start = forever do
      liftEffect $ Ref.write 0 pageNumberRef
      void $ AVar.tryTake contractsAVar
      previousContracts <- liftEffect $ Ref.read contractsRef
      nextContracts :: Map ContractId GetContractsResponse <-
        map contractsById $ Effect.liftEither =<< foldMapMContractPages @String serverUrl api range \pageContracts -> do
          let
            pageContracts' = filter filterContracts pageContracts
          liftEffect do
            let
              cs :: Map ContractId GetContractsResponse
              cs = contractsById pageContracts'
            Ref.modify_ (Map.union cs) contractsRef
            for_ (Map.additions (Map.Old previousContracts) (Map.New cs)) $ Subscription.notify listener <<< Addition
            for_ (Map.updates (Map.Old previousContracts) (Map.New cs)) $ Subscription.notify listener <<< Update
          pageNumber <- liftEffect $ Ref.modify (add 1) pageNumberRef
          delay requestInterval
          pure
            { result: pageContracts'
            , stopFetching: case possibleMaxPages of
                Nothing -> false
                Just (MaxPages maxPages) -> pageNumber >= maxPages
            }
      liftEffect do
        Ref.write nextContracts contractsRef
        for_ (Map.deletions (Map.Old previousContracts) (Map.New nextContracts)) $ Subscription.notify listener <<< Deletion
      AVar.put nextContracts contractsAVar
      delay pollingInterval

  pure $ ContractStream
    { emitter
    , getLiveState: Ref.read contractsRef
    , getState: AVar.read contractsAVar
    , start
    }

-- | The input set of endpoints which should be used for quering transactions.
type TransactionsEndpointsSource = Map ContractId TransactionsEndpoint

-- | The resuling set of txs per contract.
type ContractTransactionsMap = Map ContractId (Array TxHeaderWithEndpoint)

type ContractTransactionsEvent
  = ContractId
  /\ { new :: Array TxHeaderWithEndpoint, old :: Maybe (Array TxHeaderWithEndpoint) }

newtype ContractTransactionsStream = ContractTransactionsStream
  { emitter :: Subscription.Emitter ContractTransactionsEvent
  , getLiveState :: Effect ContractTransactionsMap
  , getState :: Aff ContractTransactionsMap
  , start :: Aff Unit
  }

-- | FIXME: take closer at error handling woudn't this component break in the case of network error?
contractsTransactions
  :: PollingInterval
  -> RequestInterval
  -> Aff TransactionsEndpointsSource
  -> ServerURL
  -> Aff ContractTransactionsStream
contractsTransactions (PollingInterval pollingInterval) requestInterval getEndpoints serverUrl = do
  stateRef <- liftEffect $ Ref.new Map.empty
  stateAVar <- AVar.empty

  { emitter, listener } <- liftEffect Subscription.create

  let
    start = forever do
      void $ AVar.tryTake stateAVar
      previousState <- liftEffect $ Ref.read stateRef
      endpoints <- getEndpoints
      { contractsTransactions: newState, notify } <- fetchContractsTransactions endpoints previousState listener requestInterval serverUrl

      liftEffect do
        Ref.write newState stateRef
        notify
      AVar.put newState stateAVar
      delay pollingInterval

  pure $ ContractTransactionsStream
    { emitter
    , getLiveState: Ref.read stateRef
    , getState: AVar.read stateAVar
    , start
    }

fetchContractsTransactions
  :: TransactionsEndpointsSource
  -> ContractTransactionsMap
  -> Listener ContractTransactionsEvent
  -> RequestInterval
  -> ServerURL
  -> Aff
       { contractsTransactions :: ContractTransactionsMap
       , notify :: Effect Unit
       }
fetchContractsTransactions endpoints prevContractTransactionMap listener (RequestInterval requestInterval) serverUrl = do
  items <- map Map.catMaybes $ forWithIndex endpoints \contractId transactionEndpoint -> do
    let
      action = do
        let
          getTransactions = do
            pages <- getPages' @String serverUrl transactionEndpoint Nothing >>= Effect.liftEither
            pure $ foldMap _.page pages
        (txHeaders :: Array { resource :: TxHeader, links :: { transaction :: TransactionEndpoint } }) <- getTransactions
        delay requestInterval
        let
          prevTransactions = Map.lookup contractId prevContractTransactionMap
          newTransactions = txHeaders <#> \{ resource, links: { transaction: transactionEndpoint' }} ->
            resource /\ transactionEndpoint'
          change =
            if Just (map fst newTransactions) == (map fst <$> prevTransactions) then
              Nothing
            else
              Just { old: prevTransactions, new: newTransactions }
        pure $ Just $ change /\ contractId /\ newTransactions
    action `catchError` \_ -> do
      pure Nothing

  let
    doNotify =
      for_ items $ case _ of
        (Just change /\ contractId /\ _) -> do
          Subscription.notify listener (contractId /\ change)
        _ -> pure unit

  pure
    { contractsTransactions: Map.fromFoldable (items <#> snd)
    , notify: doNotify
    }

-- | The input set of endpoints which should be used for quering transactions.
type ContractEndpointsSource = Map ContractId ContractEndpoint

-- | The resuling set of txs per contract.
type ContractStateMap = Map ContractId ContractState

type ContractStateEvent = ContractId /\ { new :: ContractState, old :: Maybe ContractState }

newtype ContractStateStream = ContractStateStream
  { emitter :: Subscription.Emitter ContractStateEvent
  , getLiveState :: Effect ContractStateMap
  , getState :: Aff ContractStateMap
  , start :: Aff Unit
  }

-- | FIXME: the same as above - take closer at error handling woudn't this component break in the case of network error?
contractsStates
  :: PollingInterval
  -> RequestInterval
  -> Aff ContractEndpointsSource
  -> ServerURL
  -> Aff ContractStateStream
contractsStates (PollingInterval pollingInterval) requestInterval getEndpoints serverUrl = do
  stateRef <- liftEffect $ Ref.new Map.empty
  stateAVar <- AVar.empty

  { emitter, listener } <- liftEffect Subscription.create

  let
    start = forever do
      void $ AVar.tryTake stateAVar
      previousState <- liftEffect $ Ref.read stateRef
      endpoints <- getEndpoints
      { contractsStates: newState, notify } <- fetchContractsStates endpoints previousState listener requestInterval serverUrl

      liftEffect do
        Ref.write newState stateRef
        notify
      AVar.put newState stateAVar

      delay pollingInterval
  pure $ ContractStateStream
    { emitter
    , getLiveState: Ref.read stateRef
    , getState: AVar.read stateAVar
    , start
    }

fetchContractsStates
  :: ContractEndpointsSource
  -> ContractStateMap
  -> Listener ContractStateEvent
  -> RequestInterval
  -> ServerURL
  -> Aff
       { contractsStates :: ContractStateMap
       , notify :: Effect Unit
       }
fetchContractsStates endpoints prevContractStateMap listener (RequestInterval requestInterval) serverUrl = do
  items <- map Map.catMaybes $ forWithIndex endpoints \contractId endpoint -> do
    let
      action = do
        let
          getContractState = (getResource' @String serverUrl endpoint {} >>= Effect.liftEither) <#> _.payload.resource -- <#> foldMap _.page
        (newContractState :: ContractState) <- getContractState
        delay requestInterval
        let
          oldContractState = Map.lookup contractId prevContractStateMap
          change = if oldContractState /= Just newContractState
            then Nothing
            else pure { old: oldContractState, new: newContractState }
        pure $ Just $ change /\ contractId /\ newContractState
    action `catchError` \_ -> do
      pure Nothing

  let
    doNotify =
      for_ items $ case _ of
        (Just change /\ contractId /\ _) -> do
          Subscription.notify listener (contractId /\ change)
        _ -> pure unit

  pure
    { contractsStates: Map.fromFoldable (items <#> snd)
    , notify: doNotify
    }

type TxHeaderWithEndpoint = TxHeader /\ TransactionEndpoint

type ContractWithTransactions =
  { contract :: GetContractsResponse
  -- | This fetch is done for every contract
  -- | but we don't want to wait with the updates
  -- | until all the states are fetched.
  , contractState :: Maybe GetContractResponse
  , transactions :: Array TxHeaderWithEndpoint
  }

type ContractWithTransactionsMap = Map ContractId ContractWithTransactions

data ContractWithTransactionsEvent
  = ContractEvent ContractEvent
  | ContractStateEvent ContractStateEvent
  | ContractTransactionsEvent ContractTransactionsEvent

newtype ContractWithTransactionsStream = ContractWithTransactionsStream
  { emitter :: Subscription.Emitter ContractWithTransactionsEvent
  , getLiveState :: Effect ContractWithTransactionsMap
  , getState :: Aff ContractWithTransactionsMap
  , start :: Aff Unit
  }

contractsWithTransactions :: ContractStream -> ContractStateStream -> ContractTransactionsStream -> ContractWithTransactionsStream
contractsWithTransactions (ContractStream contractStream) (ContractStateStream contractStateStream) (ContractTransactionsStream contractTransactionsStream) = do
  let
    getLiveState = do
      contractMap <- contractStream.getLiveState
      contractTransactionsMap <- contractTransactionsStream.getLiveState
      contractStateMap <- contractStateStream.getLiveState

      forWithIndex contractMap \contractId contract -> do
        let
          transactions = fromMaybe [] $ Map.lookup contractId contractTransactionsMap
          contractState = Map.lookup contractId contractStateMap
        pure { contract, contractState, transactions }

    getState = do
      contractMap <- contractStream.getState
      contractStateMap <- contractStateStream.getState
      contractTransactionsMap <- contractTransactionsStream.getState

      forWithIndex contractMap \contractId contract -> do
        let
          transactions = fromMaybe [] $ Map.lookup contractId contractTransactionsMap
          contractState = Map.lookup contractId contractStateMap
        pure { contract, contractState, transactions }

    emitter = (ContractEvent <$> contractStream.emitter)
      <|> (ContractTransactionsEvent <$> contractTransactionsStream.emitter)
      <|> (ContractStateEvent <$> contractStateStream.emitter)

    start = map (const unit) $ parSequence
      [ void $ contractStateStream.start
      , void $ contractTransactionsStream.start
      , void $ contractStream.start
      ]

  ContractWithTransactionsStream { emitter, getLiveState, getState, start }

mkContractsWithTransactions :: PollingInterval -> RequestInterval -> (GetContractsResponse -> Boolean) -> Maybe MaxPages -> ServerURL -> Aff ContractWithTransactionsStream
mkContractsWithTransactions pollingInterval requestInterval filterContracts possibleMaxPages serverUrl = do
  contractStream@(ContractStream { getState }) <- contracts pollingInterval requestInterval filterContracts possibleMaxPages serverUrl
  let
    transactionEndpointsSource = Map.catMaybes <<< map (_.links.transactions) <<< Map.filter filterContracts <$> getState
    contractEndpointsSource = map (_.links.contract) <<< Map.filter filterContracts <$> getState

  contractStateStream <- contractsStates
    pollingInterval
    requestInterval
    contractEndpointsSource
    serverUrl

  contractTransactionsStream <- contractsTransactions
    pollingInterval
    requestInterval
    transactionEndpointsSource
    serverUrl

  pure $ contractsWithTransactions contractStream contractStateStream contractTransactionsStream
