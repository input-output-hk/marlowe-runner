module Component.ContractList where

import Prelude

import Cardano as Cardano
import CardanoMultiplatformLib (CborHex, bech32ToString)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs as ApplyInputs
import Component.ApplyInputs.Machine (mkEnvironment)
import Component.ApplyInputs.Machine as ApplyInputs.Machine
import Component.ContractDetails as ContractDetails
import Component.ContractTemplates.ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import Component.ContractTemplates.Escrow as Escrow
import Component.ContractTemplates.Swap as Swap
import Component.CreateContract (runnerTag)
import Component.CreateContract as CreateContract
import Component.InputHelper (addressesInContract, canInput, rolesInContract)
import Component.Types (ContractInfo(..), ContractJsonString, MessageContent(..), MessageHub(..), MkComponentM, Page(..), WalletInfo)
import Component.Types.ContractInfo (ContractStatus(..), MarloweInfo(..), SomeContractInfo(..), contractStatusMarloweInfo, contractStatusTransactionsHeadersWithEndpoints)
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (buttonOutlinedInactive, buttonOutlinedPrimary, buttonOutlinedWithdraw)
import Component.Withdrawals as Withdrawals
import Contrib.Data.JSDate (toLocaleDateString, toLocaleTimeString) as JSDate
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder')
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec)
import Contrib.React.Svg (loadingSpinnerLogo)
import Contrib.ReactBootstrap.DropdownButton (dropdownButton)
import Contrib.ReactBootstrap.DropdownItem (dropdownItem)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (StatelessBootstrapFormSpec, textInput)
import Control.Alt ((<|>))
import Control.Alternative as Alternative
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson)
import Data.Array (catMaybes, elem, filter)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime.Instant (Instant, unInstant)
import Data.DateTime.Instant as Instant
import Data.Either (Either, hush)
import Data.Foldable (fold, for_, or)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Function (on)
import Data.JSDate (fromDateTime) as JSDate
import Data.List (intercalate)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains, length)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.These (These, maybeThese, theseLeft, theseRight)
import Data.Time.Duration (Milliseconds(..), negateDuration)
import Data.Time.Duration as Duration
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Foreign.Object as Object
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Types (Payout(..), PutTransactionRequest(..), ServerURL, Tags(..), TransactionEndpoint, TransactionsEndpoint, TxOutRef, toTextEnvelope, txOutRefToString)
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Validator (liftFnM)
import Promise.Aff as Promise
import React.Basic (fragment)
import React.Basic.DOM (br, img, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.DOM.Simplified.ToJSX (class ToJSX)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, readRef, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Table (striped) as Table
import ReactBootstrap.Table (table)
import ReactBootstrap.Types (placement)
import Utils.React.Basic.Hooks (useMaybeValue, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))
import Web.Clipboard as Clipboard

type ContractId = TxOutRef

type ValidationError = String

data FormState
  = NotValidated
  | Failure ValidationError
  | Validated (Contract)

-- An example of a simple "custom hook"
useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

type SubmissionError = String

newtype NotSyncedYetInserts = NotSyncedYetInserts
  { add :: ContractInfo.ContractCreated -> Effect Unit
  , update :: ContractInfo.ContractUpdated -> Effect Unit
  }

type Props =
  { walletInfo :: WalletInfo Wallet.Api
  , walletContext :: WalletContext
  , possibleContracts :: Maybe (Array SomeContractInfo) -- `Maybe` indicates if the contracts where fetched already
  , contractMapInitialized :: Boolean
  , notSyncedYetInserts :: NotSyncedYetInserts
  , possibleInitialModalAction :: Maybe ModalAction
  , setPage :: Page -> Effect Unit
  , submittedWithdrawalsInfo :: Map ContractId (Array TxOutRef) /\ (ContractId -> TxOutRef -> Effect Unit)
  }

data OrderBy
  = OrderByCreationDate
  | OrderByLastUpdateDate

derive instance Eq OrderBy

submit :: CborHex TransactionWitnessSetObject -> ServerURL -> TransactionEndpoint -> Aff (Either FetchError Unit)
submit witnesses serverUrl transactionEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""

    req = PutTransactionRequest textEnvelope
  put' serverUrl transactionEndpoint req

data ContractTemplate = Escrow | Swap | ContractForDifferencesWithOracle

derive instance Eq ContractTemplate

data ModalAction
  = NewContract (Maybe ContractJsonString)
  | ContractDetails
      { contract :: Maybe V1.Contract
      , state :: Maybe V1.State
      , initialContract :: V1.Contract
      , initialState :: V1.State
      , transactionEndpoints :: Array Runtime.TransactionEndpoint
      , contractId :: Runtime.ContractId
      }
  | ApplyInputs ContractInfo TransactionsEndpoint ApplyInputs.Machine.MarloweContext
  | Withdrawal WalletContext TxOutRef (NonEmptyArray Payout)
  | ContractTemplate ContractTemplate

derive instance Eq ModalAction

queryFieldId :: FieldId
queryFieldId = FieldId "query"

mkForm :: (Maybe String -> Effect Unit) -> StatelessBootstrapFormSpec Effect Query { query :: Maybe String }
mkForm onFieldValueChange = evalBuilder' ado
  query <- textInput
    { validator: liftFnM \value -> do
        onFieldValueChange value -- :: Batteries.Validator Effect _ _  _
        pure value
    , name: Just queryFieldId
    , placeholder: "Filter contracts..."
    }
  in
    { query }

actionIconSizing :: String
actionIconSizing = " h4"

runnerTags :: Tags -> Array String
runnerTags (Tags metadata) = case Map.lookup runnerTag metadata >>= decodeJson >>> hush of
  Just arr ->
    Array.filter ((_ > 2) <<< length) -- ignoring short tags

      $ arr
  Nothing -> []

someContractTags :: SomeContractInfo -> Array String
someContractTags (SyncedConractInfo (ContractInfo { tags })) = runnerTags tags
someContractTags (NotSyncedUpdatedContract { contractInfo }) = do
  let
    ContractInfo { tags } = contractInfo
  runnerTags tags
someContractTags (NotSyncedCreatedContract { tags }) = runnerTags tags

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  MessageHub msgHubProps <- asks _.msgHub
  browserCapabilities <- asks _.browserCapabilities
  createContractComponent <- CreateContract.mkComponent
  applyInputsComponent <- ApplyInputs.mkComponent
  withdrawalsComponent <- Withdrawals.mkComponent
  contractDetails <- ContractDetails.mkComponent
  escrowComponent <- Escrow.mkComponent
  swapComponent <- Swap.mkComponent
  contractForDifferencesWithOracleComponent <- ContractForDifferencesWithOracle.mkComponent

  initialEnvironment <- liftEffect $ mkEnvironment

  liftEffect $ component "ContractList" \props@{ walletInfo, walletContext, possibleInitialModalAction, possibleContracts, contractMapInitialized, submittedWithdrawalsInfo } -> React.do
    let
      NotSyncedYetInserts notSyncedYetInserts = props.notSyncedYetInserts

    environment /\ setEnvironment <- useState' initialEnvironment

    possibleModalAction /\ setModalAction /\ resetModalAction <- React.do
      p /\ set /\ reset <- useMaybeValue possibleInitialModalAction
      let
        set' = case _ of
          action@(NewContract possibleJson) -> do
            props.setPage (CreateContractPage possibleJson)
            set action
          action -> do
            props.setPage OtherPage
            set action
        reset' = do
          props.setPage ContractListPage
          reset
      useEffectOnce do
        for_ possibleInitialModalAction set'
        pure $ pure unit
      pure (p /\ set' /\ reset')

    possibleModalActionRef <- useStateRef' possibleModalAction
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }
    possibleQueryValue /\ setQueryValue <- useState' Nothing

    let
      form = mkForm setQueryValue
    { formState } <- useStatelessFormSpec
      { spec: form
      , onSubmit: const $ pure unit
      , validationDebounce: Duration.Seconds 0.5
      }
    let
      possibleContracts' :: Maybe (Array SomeContractInfo)
      possibleContracts' = do
        contracts <- possibleContracts
        let
          sortedContracts = case ordering.orderBy of
            OrderByCreationDate -> Array.sortBy (compare `on` ContractInfo.createdAt) contracts
            OrderByLastUpdateDate -> Array.sortBy (compare `on` ContractInfo.updatedAt) contracts
        pure $
          if ordering.orderAsc then sortedContracts
          else Array.reverse sortedContracts

      possibleContracts'' = do
        let
          filtered = do
            queryValue <- possibleQueryValue
            contracts <- possibleContracts'
            pure $ contracts # Array.filter \someContract -> do
              let
                contractTags = ContractInfo.someContractTags someContract
                tagList = runnerTags (contractTags :: Tags)
                contractId = ContractInfo.someContractContractId someContract
                pattern = Pattern queryValue
              contains pattern (txOutRefToString contractId) || or (map (contains pattern) tagList)
        filtered <|> possibleContracts'
      nextTimeouts = fromMaybe [] do
        contracts <- possibleContracts''
        pure $ Array.sort $ catMaybes $ flip map contracts $ ContractInfo.someContractCurrentContract >=> case _ of
          V1.When _ timeout _ -> pure timeout
          _ -> Nothing

    -- Trigger auto refresh on timeouts so we can advance through the contract
    setCurrTimeout <- snd <$> useState' Nothing
    useAff nextTimeouts do
      now <- liftEffect Now.now
      for_ nextTimeouts \timeout -> do
        let
          nextTimeoutDelay = (unInstant timeout <> negateDuration (unInstant now))
        when (nextTimeoutDelay > Milliseconds 0.0) do
          delay (nextTimeoutDelay <> Milliseconds 1000.0)
          liftEffect $ mkEnvironment >>= setEnvironment
          liftEffect $ setCurrTimeout $ Just timeout

    pure do
      let
        onError error = do
          msgHubProps.add $ Error $ DOOM.text $ fold [ "An error occured during contract submission: " <> error ]
          resetModalAction
      case possibleModalAction, submittedWithdrawalsInfo of
        Just (NewContract possibleInitialContract), _ -> createContractComponent
          { connectedWallet: walletInfo
          , walletContext
          , onDismiss: resetModalAction
          , onSuccess: \contractCreated -> do
              msgHubProps.add $ Success $ DOOM.text $ String.joinWith " "
                [ "Successfully created and submitted the contract."
                , "Contract transaction awaits to be included in the blockchain."
                ]
              notSyncedYetInserts.add contractCreated
              resetModalAction
          , onError
          , possibleInitialContract
          }
        Just (ApplyInputs contractInfo transactionsEndpoint marloweContext), _ -> do
          let
            onSuccess = \contractUpdated -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." ]
              notSyncedYetInserts.update contractUpdated
              resetModalAction
          applyInputsComponent
            { transactionsEndpoint
            , contractInfo
            , marloweContext
            , onError
            , connectedWallet: walletInfo
            , onSuccess
            , onDismiss: resetModalAction
            }
        Just (Withdrawal _ contractId unclaimedPayouts), _ /\ updateSubmitted -> do
          let
            onSuccess = \_ -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully withdrawed the funds. Withdrawal transaction awaits to be included in the blockchain." ]
              resetModalAction
          withdrawalsComponent
            { connectedWallet: walletInfo
            , onSuccess
            , onError
            , onDismiss: resetModalAction
            , unclaimedPayouts
            , updateSubmitted: updateSubmitted contractId
            , walletContext
            }
        Just (ContractDetails { contractId, contract, state, initialContract, initialState, transactionEndpoints }), _ -> do
          let
            onClose = resetModalAction
          contractDetails { contractId, contract, onClose, state, transactionEndpoints, initialContract, initialState }

        -- This should be fixed later on - for now we put some stubs
        Just (ContractTemplate Escrow), _ -> escrowComponent
          { onSuccess: \_ -> resetModalAction
          , onDismiss: resetModalAction
          }

        Just (ContractTemplate Swap), _ -> swapComponent
          { onSuccess: \_ -> resetModalAction
          , onDismiss: resetModalAction
          }

        Just (ContractTemplate ContractForDifferencesWithOracle), _ -> contractForDifferencesWithOracleComponent
          { onSuccess: \_ -> resetModalAction
          , onDismiss: resetModalAction
          }

        Nothing, _ -> React.fragment
          [ DOM.div { className: "container" } $ DOM.div { className: "row" } do
              let
                newContractButton = buttonOutlinedPrimary
                  { label: DOOM.text "Create a contract"
                  , onClick: do
                      readRef possibleModalActionRef >>= case _ of
                        Nothing -> setModalAction (NewContract Nothing)
                        _ -> pure unit
                  }
                templateContractButton = dropdownButton
                  { className: "d-none"
                  , title: fragment
                      [ Icons.toJSX $ unsafeIcon "file-earmark-medical h5 me-1"
                      , DOOM.text "Use Contract Template"
                      ]
                  -- , onToggle: const $ pure unit
                  }
                  [ dropdownItem
                      { onClick: handler_ $ setModalAction $ ContractTemplate Escrow
                      }
                      [ DOOM.text "Escrow" ]
                  , dropdownItem
                      { onClick: handler_ $ setModalAction $ ContractTemplate Swap
                      }
                      [ DOOM.text "Swap" ]
                  , dropdownItem
                      { onClick: handler_ $ setModalAction $ ContractTemplate ContractForDifferencesWithOracle
                      }
                      [ DOOM.text "Contract For Differences with Oracle" ]
                  ]
              [ DOM.div { className: "col-7 text-end" }
                  [ DOM.div { className: "form-group" } $ renderFormSpec form formState ]
              , DOM.div { className: "col-5" } $ Array.singleton $ do
                  DOM.div { className: "text-end" }
                    [ newContractButton
                    , templateContractButton
                    ]
              ]
          , do
              let
                spinner =
                  DOM.div
                    { className: "col-12 position-relative d-flex justify-content-center align-items-center blur-bg z-index-sticky" }
                    $ loadingSpinnerLogo {}
                tableHeader =
                  DOM.thead {} do
                    let
                      orderingTh = Table.orderingHeader ordering updateOrdering "background-color-primary-light border-0 text-centered text-muted"
                      th content extraClassNames = DOM.th { className: "background-color-primary-light border-0 text-center " <> extraClassNames } [ content ]
                      thWithIcon src extraClassNames = th (DOOM.img { src }) extraClassNames
                      thWithLabel label extraClassNames = th (DOOM.text label) ("text-muted " <> extraClassNames)
                    [ DOM.tr {}
                        [ thWithIcon "/images/calendar_month.svg" "rounded-top-left"
                        , thWithIcon "/images/event_available.svg" ""
                        , thWithIcon "/images/fingerprint.svg" ""
                        , thWithIcon "/images/sell.svg" ""
                        , thWithIcon "/images/frame_65.svg" "rounded-top-right"
                        ]
                    , DOM.tr { className: "border-bottom-white border-bottom-4px" }
                        [ do
                            let
                              label = DOOM.text "Created"
                            orderingTh label OrderByCreationDate
                        , do
                            let
                              label = DOOM.text "Updated"
                            orderingTh label OrderByLastUpdateDate
                        , thWithLabel "Contract Id" "w-16rem"
                        , thWithLabel "Tags" ""
                        , thWithLabel "Actions" ""
                        ]
                    ]
                mkTable tbody = table { striped: Table.striped.boolean false, hover: true, id: "contracts" }
                  [ tableHeader
                  , tbody
                  ]

              DOM.div { className: "container flex-grow-1" } $ DOM.div { className: "row" } $ DOM.div { className: "col-12" } $ DOM.div { className: "p-3 shadow-sm rounded my-3" } $ case possibleContracts'', contractMapInitialized of
                -- Pre search no started
                Nothing, _ -> fragment [ mkTable mempty, spinner ]
                -- Searching but nothing was found, still searching
                Just [], false -> fragment [ mkTable mempty, spinner ]
                -- Searching but nothing was found, search finished
                Just [], true -> fragment
                  [ mkTable mempty
                  , DOM.div { className: "container" }
                      $ DOM.div { className: "row" }
                      $ DOM.div { className: "col-12 text-center py-3 fw-bold mute" }
                      $
                        DOOM.text "No contracts associated with your wallets were found."
                  ]
                -- Searching and something was found
                Just contracts, _ -> DOM.div { className: "col-12 px-0" } do
                  let
                    tdCentered :: forall jsx. ToJSX jsx => jsx -> JSX
                    tdCentered = DOM.td { className: "text-center border-0" }
                    tdDateTime Nothing = tdCentered $ ([] :: Array JSX)
                    tdDateTime (Just dateTime) = tdCentered $ Array.singleton $ DOM.small {} do
                      let
                        jsDate = JSDate.fromDateTime dateTime
                      [ DOOM.text $ JSDate.toLocaleDateString jsDate
                      , DOOM.br {}
                      , DOOM.text $ JSDate.toLocaleTimeString jsDate
                      ]
                    tdInstant possibleInstant = do
                      let
                        possibleDateTime = Instant.toDateTime <$> possibleInstant
                      tdDateTime possibleDateTime

                    -- FIXME: At the end we want to make this view smarter and present also
                    ---       unconfirmed transactions using animated borders.
                    tdContractId contractId possibleMarloweInfo transactionEndpoints = do
                      let
                        conractIdStr = txOutRefToString contractId

                      tdCentered $ DOM.span { className: "d-flex" }
                        [ case possibleMarloweInfo of
                            Just (MarloweInfo { state, currentContract, initialContract, initialState }) -> do
                              DOM.a
                                do
                                  let
                                    onClick = setModalAction $ ContractDetails
                                      { contractId
                                      , contract: currentContract
                                      , state
                                      , initialState: initialState
                                      , initialContract: initialContract
                                      , transactionEndpoints
                                      }
                                  { className: "cursor-pointer text-decoration-none text-reset text-decoration-underline-hover d-inline-block" <> case browserCapabilities.clipboard of
                                      Just _ -> " truncate-text w-16rem"
                                      Nothing -> ""
                                  , onClick: handler_ onClick
                                  }
                                [ text conractIdStr ]
                            Nothing -> DOM.span { className: "text-muted truncate-text w-16rem" } $ text conractIdStr
                        , case browserCapabilities.clipboard of
                            Just clipboard -> do
                              let
                                copyToClipboard :: Effect Unit
                                copyToClipboard = launchAff_ (Promise.toAffE $ Clipboard.writeText conractIdStr clipboard)
                              DOM.a
                                { href: "#"
                                , onClick: handler_ copyToClipboard
                                , className: "cursor-pointer text-decoration-none text-decoration-underline-hover text-reset"
                                }
                                $ Icons.toJSX
                                $ unsafeIcon "clipboard-plus ms-1 d-inline-block"
                            Nothing -> mempty
                        ]

                  mkTable
                    $ DOM.tbody {}
                    $ contracts <#> \someContract -> do
                        let
                          createdAt = ContractInfo.createdAt someContract
                          updatedAt = ContractInfo.updatedAt someContract
                          tags = runnerTags $ ContractInfo.someContractTags someContract
                          tdTags = tdCentered
                            [ DOOM.text $ intercalate ", " tags
                            -- , DOOM.text $ unsafeStringify $ someContractInfoStatus someContract
                            ]
                          contractId = ContractInfo.someContractContractId someContract
                          actionsContext = contractActionsContext
                            environment
                            walletContext
                            (fst submittedWithdrawalsInfo)
                            someContract

                          trClassName = "align-middle border-bottom-white border-bottom-4px" <> case actionsContext of
                            AwaitingOtherParty _ -> " bg-secondary"
                            AwaitingTimeout _ -> " bg-secondary"
                            ContractComplete _ -> "bg-secondary"
                            _ -> ""

                          mkTestId testId = Object.fromHomogeneous { testId }
                          contractIdStr = txOutRefToString contractId

                          confirmedMarloweInfo = case someContract of
                            (SyncedConractInfo (ContractInfo { contractStatus })) -> contractStatusMarloweInfo contractStatus
                            _ -> Nothing
                          transactionsEndpoints = map snd <$> case someContract of
                            (SyncedConractInfo (ContractInfo { contractStatus })) -> contractStatusTransactionsHeadersWithEndpoints contractStatus
                            _ -> Just []

                        DOM.tr { className: trClassName, _data: mkTestId contractIdStr }
                          [ tdInstant createdAt
                          , tdInstant $ updatedAt <|> createdAt
                          -- FIXME: We have to distinguish unconfirmed / unsynced creation as well
                          , tdContractId contractId confirmedMarloweInfo (fold transactionsEndpoints)
                          , tdTags
                          , DOM.td { className: "text-center border-0", _data: mkTestId $ contractIdStr <> "-actions" } case actionsContext of
                              SyncingContract sc -> buttonOutlinedInactive do
                                let
                                  tooltipText = case sc of
                                    NotSyncedCreatedContract _ -> "Awaiting Runtime submission confirmation"
                                    NotSyncedUpdatedContract _ -> "Awaiting Runtime submission confirmation"
                                    SyncedConractInfo (ContractInfo { contractStatus }) -> case contractStatus of
                                      StillFetching _ -> "Still fetching contract details"
                                      NotConfirmedCreation { txStatus } -> "Runtime reports last transaction status: " <> show txStatus
                                      NotConfirmedInputsApplication { txStatus } -> "Runtime reports last transaction status: " <> show txStatus
                                      Confirmed _ -> "Transaction status confirmed" -- should never be visible
                                { label: DOOM.text $ "Syncing"
                                , tooltipText: tooltipText
                                , tooltipPlacement: placement.left
                                , disabled: true
                                }
                              AwaitingOtherParty _ -> DOM.span { className: "text-muted" } $ DOOM.text "Awaiting other party"
                              AwaitingTimeout _ -> DOM.span { className: "text-muted" } $ DOOM.text "Awaiting timeout"
                              ContractComplete _ -> DOOM.text "Complete"
                              ActionsAvailable { contractInfo, actions } -> React.fragment
                                [ case theseRight actions of
                                    Just (CanWithdraw payouts) -> do
                                      buttonOutlinedWithdraw
                                        { label: DOOM.text "Withdraw"
                                        , tooltipText: "You have funds available for withdrawal from this contract. Click to submit a withdrawal"
                                        , tooltipPlacement: placement.left
                                        , onClick: setModalAction $ Withdrawal walletContext contractId payouts
                                        }
                                    Nothing -> mempty
                                , case theseLeft actions of
                                    Just (CanAdvance { contract, initialContract, state, transactionsEndpoint }) -> buttonOutlinedPrimary
                                      { label: DOOM.text "Advance"
                                      , onClick: setModalAction $ ApplyInputs contractInfo transactionsEndpoint { initialContract, state, contract }
                                      , tooltipPlacement: placement.left
                                      , tooltipText: "You are able to execuate a step in this contract"
                                      }
                                    _ -> mempty
                                ]
                          ]
          ]

remainingPayouts :: ContractId -> Array V1.Party -> Map ContractId (Array TxOutRef) -> Array Payout -> Array Payout
remainingPayouts contractId parties submittedPayouts unclaimedPayouts = do
  let
    payouts = case lookup contractId submittedPayouts of
      Just s -> filter ((\(Payout { payoutId }) -> not (elem payoutId s))) unclaimedPayouts
      Nothing -> unclaimedPayouts

    roleNames = catMaybes $ parties <#> case _ of
      V1.Role roleName -> Just roleName
      V1.Address _ -> Nothing

  Array.filter (\(Payout { role }) -> role `elem` roleNames) payouts

walletParties
  :: WalletContext
  -> Maybe V1.CurrencySymbol
  -> V1.Contract
  -> Either String (Array V1.Party)
walletParties walletContext@(WalletContext { changeAddress, usedAddresses }) possibleCurrencySymbol contract = do
  roleParties <- case possibleCurrencySymbol of
    Nothing -> pure []
    Just currencySymbol -> case Cardano.policyIdFromHexString currencySymbol of
      Just policyId -> do
        let
          contractRoles = rolesInContract contract
          WalletContext { balance: Cardano.Value balanceValue } = walletContext
          assetsIds2Role = contractRoles <#> \tokenName -> do
            let
              assetName = Cardano.assetNameFromString tokenName
            Cardano.AssetId policyId assetName /\ V1.Role tokenName
        pure $ Array.catMaybes $ assetsIds2Role <#> \(assetId /\ role) -> do
          void $ Map.lookup assetId balanceValue
          pure role
      Nothing -> throwError "Invalid currency symbol"

  let
    contractAddresses :: Array V1.Address
    contractAddresses = addressesInContract contract

    addressParties :: Array V1.Party
    addressParties =
      map V1.Address
        <<< filter (_ `elem` contractAddresses)
        <<< map bech32ToString
        $ Array.cons changeAddress usedAddresses
  pure $ roleParties <> addressParties

data CanAdvance = CanAdvance
  { transactionsEndpoint :: Runtime.TransactionsEndpoint
  , initialContract :: V1.Contract
  , contract :: V1.Contract
  , state :: V1.State
  }

data CanWithdraw = CanWithdraw (NonEmptyArray Payout)

data ContractActionsContext
  = ActionsAvailable
      { contractInfo :: ContractInfo
      , actions :: These CanAdvance CanWithdraw
      }
  | AwaitingOtherParty ContractInfo
  | AwaitingTimeout
      { contractInfo :: ContractInfo
      , timeout :: Instant
      }
  | SyncingContract SomeContractInfo
  | ContractComplete ContractInfo

contractActionsContext :: V1.Environment -> WalletContext -> Map TxOutRef (Array TxOutRef) -> SomeContractInfo -> ContractActionsContext
contractActionsContext _ _ _ sc@(NotSyncedUpdatedContract _) = SyncingContract sc
contractActionsContext _ _ _ sc@(NotSyncedCreatedContract _) = SyncingContract sc
contractActionsContext env walletContext submittedPayouts sc@(SyncedConractInfo contractInfo) = do
  let
    ContractInfo { contractId, contractStatus } = contractInfo
    isNotify (V1.Case (V1.Notify _) _) = true
    isNotify _ = false
  case contractStatus of
    StillFetching _ -> SyncingContract sc
    NotConfirmedCreation _ -> SyncingContract sc
    NotConfirmedInputsApplication _ -> SyncingContract sc
    Confirmed { marloweInfo, endpoints } -> do
      let
        MarloweInfo { initialContract, currencySymbol, currentContract, state: currentState, unclaimedPayouts } = marloweInfo

        possiblyParties = hush $ walletParties walletContext currencySymbol initialContract
        possiblyCanWithdraw = do
          parties <- possiblyParties
          payouts <- NonEmptyArray.fromArray $ remainingPayouts contractId parties submittedPayouts unclaimedPayouts
          pure $ CanWithdraw payouts
        possiblyCanAdvance = do
          parties <- possiblyParties
          contract <- currentContract
          state <- currentState
          let
            canAdvance = CanAdvance
              { contract, state, transactionsEndpoint: endpoints.transactions, initialContract }
          Alternative.guard (Array.any (canInput env state contract) parties) $> canAdvance

        possiblyActions :: Maybe (These CanAdvance CanWithdraw)
        possiblyActions = maybeThese
          possiblyCanAdvance
          possiblyCanWithdraw

      case possiblyActions of
        Just actions -> ActionsAvailable { contractInfo, actions }
        Nothing -> case currentContract of
          Just (V1.When cases timeout _) | cases == [] || Array.all isNotify cases -> AwaitingTimeout { contractInfo, timeout }
          Nothing -> ContractComplete contractInfo
          _ -> AwaitingOtherParty contractInfo

