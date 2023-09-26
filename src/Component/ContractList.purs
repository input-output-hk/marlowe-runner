module Component.ContractList where

import Prelude

import Cardano as Cardano
import CardanoMultiplatformLib (CborHex, bech32ToString)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs as ApplyInputs
import Component.ApplyInputs.Machine as ApplyInputs.Machine
import Component.ContractDetails as ContractDetails
import Component.ContractTemplates.ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import Component.ContractTemplates.Escrow as Escrow
import Component.ContractTemplates.Swap as Swap
import Component.CreateContract (runLiteTag)
import Component.CreateContract as CreateContract
import Component.InputHelper (canInput)
import Component.Types (ContractInfo(..), ContractJsonString, MessageContent(..), MessageHub(..), MkComponentM, Page(..), WalletInfo)
import Component.Types.ContractInfo (MarloweInfo(..), SomeContractInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (buttonOutlinedInactive, buttonOutlinedPrimary, buttonWithIcon)
import Component.Withdrawals as Withdrawals
import Contrib.Data.DateTime.Instant (millisecondsFromNow)
import Contrib.Data.JSDate (toLocaleDateString, toLocaleTimeString) as JSDate
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder')
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec)
import Contrib.React.Svg (loadingSpinnerLogo)
import Contrib.ReactBootstrap.DropdownButton (dropdownButton)
import Contrib.ReactBootstrap.DropdownItem (dropdownItem)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (FormControlSizing(..), StatelessBootstrapFormSpec, textInput)
import Control.Alt ((<|>))
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, encodeJson, stringify)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime.Instant (Instant, instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either, hush)
import Data.Foldable (findMap, fold, or)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Function (on)
import Data.Int as Int
import Data.JSDate (fromDateTime) as JSDate
import Data.List (intercalate)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Monoid as Monoid
import Data.Newtype (un)
import Data.Set as Set
import Data.String (contains, length)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration as Duration
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics (isClose)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Types (ContractHeader(ContractHeader), Payout(..), PutTransactionRequest(..), Runtime(..), ServerURL, Tags(..), TransactionEndpoint, TransactionsEndpoint, TxOutRef, WithdrawalsEndpoint, toTextEnvelope, txOutRefToString)
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
import React.Basic.Hooks (Hook, JSX, UseState, component, readRef, useContext, useEffect, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap (overlayTrigger, tooltip)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Table (striped) as Table
import ReactBootstrap.Table (table)
import ReactBootstrap.Types (placement)
import ReactBootstrap.Types as OverlayTrigger
import Unsafe.Coerce (unsafeCoerce)
import Utils.React.Basic.Hooks (useMaybeValue, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))
import Web.Clipboard (clipboard)
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Window (navigator)

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
  { possibleContracts :: Maybe (Array SomeContractInfo) -- `Maybe` indicates if the contracts where fetched already
  , contractMapInitialized :: Boolean
  , notSyncedYetInserts :: NotSyncedYetInserts
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  , possibleInitialModalAction :: Maybe ModalAction
  , setPage :: Page -> Effect Unit
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
      }
  | ApplyInputs ContractInfo TransactionsEndpoint ApplyInputs.Machine.MarloweContext
  | Withdrawal WithdrawalsEndpoint (NonEmptyArray.NonEmptyArray String) TxOutRef
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

runLiteTags :: Tags -> Array String
runLiteTags (Tags metadata) = case Map.lookup runLiteTag metadata >>= decodeJson >>> hush of
  Just arr ->
    Array.filter ((_ > 2) <<< length) -- ignoring short tags

      $ arr
  Nothing -> []

someContractTags :: SomeContractInfo -> Array String
someContractTags (SyncedConractInfo (ContractInfo { tags })) = runLiteTags tags
someContractTags (NotSyncedUpdatedContract { contractInfo }) = do
  let
    ContractInfo { tags } = contractInfo
  runLiteTags tags
someContractTags (NotSyncedCreatedContract { tags }) = runLiteTags tags

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  MessageHub msgHubProps <- asks _.msgHub
  Runtime runtime <- asks _.runtime
  walletInfoCtx <- asks _.walletInfoCtx

  createContractComponent <- CreateContract.mkComponent
  applyInputsComponent <- ApplyInputs.mkComponent
  withdrawalsComponent <- Withdrawals.mkComponent
  contractDetails <- ContractDetails.mkComponent
  escrowComponent <- Escrow.mkComponent
  swapComponent <- Swap.mkComponent
  contractForDifferencesWithOracleComponent <- ContractForDifferencesWithOracle.mkComponent

  invalidBefore <- liftEffect $ millisecondsFromNow (Duration.Milliseconds (Int.toNumber $ (-10) * 60 * 1000))
  invalidHereafter <- liftEffect $ millisecondsFromNow (Duration.Milliseconds (Int.toNumber $ 5 * 60 * 1000))

  liftEffect $ component "ContractList" \props@{ connectedWallet, possibleInitialModalAction, possibleContracts, contractMapInitialized } -> React.do
    let
      NotSyncedYetInserts notSyncedYetInserts = props.notSyncedYetInserts

    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)

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
          props.setPage ContarctListPage
          reset
      pure (p /\ set' /\ reset')

    possibleModalActionRef <- useStateRef' possibleModalAction
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }
    possibleQueryValue /\ setQueryValue <- useState' Nothing

    useEffect possibleContracts do
      let
        possibleApplyInputs :: Maybe ModalAction
        possibleApplyInputs = do
          contracts <- possibleContracts
          let
            txIdStr = "e99a290e6f8a165ceab3fd767401d391d10dfec48d842e1676484beb2e5e31bd"
            txIx = 1
            contractId = Runtime.TxOutRef
              { txId: Runtime.TxId txIdStr
              , txIx
              }
          ci@(ContractInfo contractInfo) <- flip findMap contracts case _ of
            SyncedConractInfo ci@(ContractInfo { contractId: contractId' }) ->
              if contractId == contractId' then Just ci
              else Nothing
            _ -> Nothing
          MarloweInfo marloweInfo <- contractInfo.marloweInfo
          state <- marloweInfo.state
          contract <- marloweInfo.currentContract
          let
            marloweContext = { initialContract: marloweInfo.initialContract, state, contract }
            contractIdParam = txIdStr <> "%23" <> show txIx
            transactionsEndpoint = unsafeCoerce $ "/contracts/" <> contractIdParam <> "/transactions"
          pure $ ApplyInputs
            ci
            transactionsEndpoint
            marloweContext
      -- case possibleApplyInputs of
      --   Just applyInputs -> do
      --     setModalAction $ applyInputs
      --   Nothing -> pure unit
      pure $ pure unit

    let
      form = mkForm setQueryValue
    { formState } <- useStatelessFormSpec
      { spec: form
      , onSubmit: const $ pure unit
      , validationDebounce: Duration.Seconds 0.5
      }
    let
      isContractComplete Nothing = true
      isContractComplete (Just V1.Close) = true
      isContractComplete _ = false

      possibleContracts' :: Maybe (Array SomeContractInfo)
      possibleContracts' = do
        contracts <- possibleContracts
        let
          -- FIXME: Quick and dirty hack to display just submited contracts as first - `Nothing ` is lower than `Just`
          -- someFutureBlockNumber = Runtime.BlockNumber 129058430
          sortedContracts = case ordering.orderBy of
            OrderByCreationDate -> Array.sortBy (compare `on` ContractInfo.createdAt) contracts
            OrderByLastUpdateDate -> Array.sortBy (compare `on` ContractInfo.updatedAt) contracts
        -- OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contracts
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
                tagList = runLiteTags (contractTags :: Tags)
                contractId = ContractInfo.someContractContractId someContract
                pattern = Pattern queryValue
              contains pattern (txOutRefToString contractId) || or (map (contains pattern) tagList)
        filtered <|> possibleContracts'

    pure $ DOM.div { className: "min-height-100vh" } do
      case possibleModalAction, connectedWallet of
        Just (NewContract possibleInitialContract), Just cw -> createContractComponent
          { connectedWallet: cw
          , onDismiss: resetModalAction
          , onSuccess: \contractCreated -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain."
                , "Contract status should change to 'Confirmed' at that point."
                ]
              resetModalAction
              notSyncedYetInserts.add contractCreated
          , possibleInitialContract
          }
        Just (ApplyInputs contractInfo transactionsEndpoint marloweContext), Just cw -> do
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
            , connectedWallet: cw
            , onSuccess
            , onDismiss: resetModalAction
            }
        Just (Withdrawal withdrawalsEndpoint roles contractId), Just cw -> do
          let
            onSuccess = \_ -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." ]
              resetModalAction
          withdrawalsComponent
            { inModal: true
            , withdrawalsEndpoint
            , roles
            , contractId
            , connectedWallet: cw
            , onSuccess
            , onDismiss: resetModalAction
            }
        Just (ContractDetails { contract, state, initialContract, initialState, transactionEndpoints }), _ -> do
          let
            onClose = resetModalAction
          contractDetails { contract, onClose, state, transactionEndpoints, initialContract, initialState }

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
                disabled = isNothing connectedWallet
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
                  let
                    buttons = DOM.div { className: "text-end" }
                      [ newContractButton
                      , templateContractButton
                      ]
                  if disabled then do
                    let
                      tooltipJSX = tooltip
                        { placement: placement.left }
                        (DOOM.text "Connect to a wallet to add a contract")
                    overlayTrigger
                      { overlay: tooltipJSX
                      , placement: OverlayTrigger.placement.bottom
                      }
                      -- Disabled button doesn't trigger the hook,
                      -- so we wrap it in a `span`
                      buttons
                  else
                    buttons
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
                      orderingTh = Table.orderingHeader ordering updateOrdering "background-color-primary-light border-0 text-centered"
                      th content extraClassNames = DOM.th { className: "background-color-primary-light border-0 text-center " <> extraClassNames } [ content ]
                      thWithIcon src extraClassNames = th (DOOM.img { src }) extraClassNames
                      thWithLabel label extraClassNames = th (DOOM.text label) ("text-muted " <> extraClassNames)
                    [ DOM.tr {}
                        [ thWithIcon "/images/calendar_month.svg" "rounded-top"
                        , thWithIcon "/images/event_available.svg" ""
                        , thWithIcon "/images/fingerprint.svg" ""
                        , thWithIcon "/images/sell.svg" ""
                        , thWithIcon "/images/frame_65.svg" "rounded-top"
                        ]
                    , DOM.tr {}
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
                mkTable tbody = table { striped: Table.striped.boolean false, hover: true }
                  [ tableHeader
                  , tbody
                  ]

              DOM.div { className: "container" } $ DOM.div { className: "row" } $ DOM.div { className: "col-12" } $ DOM.div { className: "p-3 shadow rounded my-3" } $ case possibleContracts'', contractMapInitialized of
                -- Pre search no started
                Nothing, _ -> fragment [ mkTable mempty, spinner ]
                -- Searching but nothing was found, still searching
                Just [], false -> fragment [ mkTable mempty, spinner ]
                -- Searching but nothing was found, search finished
                Just [], true -> fragment
                  [ mkTable mempty
                  , DOM.div { className: "container" }
                      $ DOM.div { className: "row" }
                      $ DOM.div { className: "col-12 text-center py-3 fw-bold" }
                      $
                        DOOM.text "No contracts found"
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

                    tdContractId contractId possibleMarloweInfo transactionEndpoints = do
                      let
                        conractIdStr = txOutRefToString contractId

                        copyToClipboard :: Effect Unit
                        copyToClipboard = window >>= navigator >>= clipboard >>= \c -> do
                          launchAff_ (Promise.toAffE $ Clipboard.writeText conractIdStr c)

                      tdCentered $ DOM.span { className: "d-flex" }
                        [ DOM.a
                            do
                              let
                                onClick = case possibleMarloweInfo of
                                  Just (MarloweInfo { state, currentContract, initialContract, initialState }) -> do
                                    setModalAction $ ContractDetails
                                      { contract: currentContract
                                      , state
                                      , initialState: initialState
                                      , initialContract: initialContract
                                      , transactionEndpoints
                                      }
                                  _ -> pure unit
                              { className: "cursor-pointer text-decoration-none text-reset text-decoration-underline-hover truncate-text w-16rem d-inline-block"
                              , onClick: handler_ onClick
                              -- , disabled
                              }
                            [ text conractIdStr ]
                        , DOM.a
                            { href: "#"
                            , onClick: handler_ copyToClipboard
                            , className: "cursor-pointer text-decoration-none text-decoration-underline-hover text-reset"
                            }
                            $ Icons.toJSX
                            $ unsafeIcon "clipboard-plus ms-1 d-inline-block"
                        ]
                  mkTable
                    $ DOM.tbody {}
                    $ contracts <#> \someContract -> do
                        let
                          createdAt = ContractInfo.createdAt someContract
                          updatedAt = ContractInfo.updatedAt someContract
                          tags = runLiteTags $ ContractInfo.someContractTags someContract
                          contractId = ContractInfo.someContractContractId someContract
                          possibleContract = ContractInfo.someContractCurrentContract someContract
                          isClosed = isContractComplete possibleContract
                          trClassName = if isClosed then "align-middle bg-secondary border-bottom-white-4px" else "align-middle border-bottom-white-4px"

                        DOM.tr { className: trClassName } $ case someContract of
                          (SyncedConractInfo ci@(ContractInfo { _runtime, endpoints, marloweInfo })) -> do
                            let
                              ContractHeader { contractId } = _runtime.contractHeader
                            [ tdInstant createdAt
                            , tdInstant $ updatedAt <|> createdAt
                            , do
                                let
                                  transactionEndpoints = _runtime.transactions <#> \(_ /\ transactionEndpoint) -> transactionEndpoint
                                tdContractId contractId marloweInfo transactionEndpoints
                            , tdCentered [ DOOM.text $ intercalate ", " tags ]
                            , tdCentered
                                [ do
                                    -- FIXME: Usage of inputs roles doesn't exclude addresses usage. Combine these two branches into one and probably
                                    -- simplify the check if currency symbol is `Nothing`.
                                    case endpoints.transactions, marloweInfo, possibleWalletContext of
                                      Just transactionsEndpoint,
                                      Just (MarloweInfo { currencySymbol: Just currencySymbol, initialContract, state: Just state, currentContract: Just contract }),
                                      Just { balance: Cardano.Value balance } -> do
                                        let
                                          timeInterval = V1.TimeInterval invalidBefore invalidHereafter
                                          environment = V1.Environment { timeInterval }
                                          balance' = Map.filterKeys (\assetId -> Cardano.assetIdToString assetId `eq` currencySymbol) balance
                                          roles = catMaybes <<< map assetToString <<< List.toUnfoldable <<< Set.toUnfoldable <<< Map.keys $ balance'
                                        Monoid.guard
                                          (Array.any (\role -> canInput (V1.Role role) environment state contract) roles)
                                          buttonOutlinedPrimary
                                            { label: DOOM.text "Advance"
                                            , extraClassNames: "me-2"
                                            , onClick: do
                                                let
                                                  marloweContext = { initialContract, state, contract }
                                                setModalAction $ ApplyInputs ci transactionsEndpoint marloweContext
                                            }
                                      Just transactionsEndpoint,
                                      Just (MarloweInfo { initialContract, state: Just state, currentContract: Just contract }),
                                      Just { usedAddresses } -> do
                                        let
                                          timeInterval = V1.TimeInterval invalidBefore invalidHereafter
                                          environment = V1.Environment { timeInterval }
                                          marloweContext = { initialContract, state, contract }

                                        Monoid.guard
                                          (Array.any (\addr -> canInput (V1.Address $ bech32ToString addr) environment state contract) usedAddresses)
                                          buttonOutlinedPrimary
                                            { label: DOOM.text "Advance"
                                            , extraClassNames: "font-weight-bold btn-outline-primary"
                                            , onClick: setModalAction $ ApplyInputs ci transactionsEndpoint marloweContext
                                            }
                                      _, Just (MarloweInfo { state: Nothing, currentContract: Nothing }), _ -> DOOM.text "Complete"
                                      _, _, _ -> buttonOutlinedInactive { label: DOOM.text "Syncing" }

                                , case marloweInfo, possibleWalletContext of
                                    Just (MarloweInfo { currencySymbol: Just currencySymbol, state: _, unclaimedPayouts }), Just { balance: Cardano.Value balance } -> do
                                      let
                                        balance' = Map.filterKeys (\assetId -> Cardano.assetIdToString assetId `eq` currencySymbol) balance
                                        roleTokens = map Cardano.assetIdToString <<< List.toUnfoldable <<< Set.toUnfoldable <<< Map.keys $ balance'
                                      case Array.uncons (Array.intersect roleTokens (map (\(Payout { role }) -> role) unclaimedPayouts)) of
                                        Just { head, tail } -> buttonWithIcon
                                          { icon: unsafeIcon mempty
                                          , label: DOOM.text "Withdraw"
                                          , extraClassNames: "font-weight-bold me-2 btn-outline-warning"
                                          , tooltipText: Just "This wallet has funds available for withdrawal from this contract. Click to submit a withdrawal"
                                          , onClick: setModalAction $ Withdrawal runtime.withdrawalsEndpoint (NonEmptyArray.cons' head tail) contractId
                                          }
                                        _ -> mempty
                                    _, _ -> mempty
                                , case marloweInfo, possibleWalletContext of
                                    Just (MarloweInfo { currencySymbol: Just currencySymbol, state: _, unclaimedPayouts }), Just { balance: Cardano.Value balance } -> do
                                      let
                                        balance' = Map.filterKeys (\assetId -> Cardano.assetIdToString assetId `eq` currencySymbol) balance
                                        roleTokens = map Cardano.assetIdToString <<< List.toUnfoldable <<< Set.toUnfoldable <<< Map.keys $ balance'
                                      case Array.uncons (Array.intersect roleTokens (map (\(Payout { role }) -> role) unclaimedPayouts)) of
                                        Just { head, tail } -> buttonWithIcon
                                          { icon: unsafeIcon mempty
                                          , label: DOOM.text "Withdraw"
                                          , extraClassNames: "font-weight-bold me-2 btn-outline-warning"
                                          , tooltipText: Just "This wallet has funds available for withdrawal from this contract. Click to submit a withdrawal"
                                          , onClick: setModalAction $ Withdrawal runtime.withdrawalsEndpoint (NonEmptyArray.cons' head tail) contractId
                                          }
                                        _ -> mempty
                                    _, _ -> mempty
                                ]
                            ]
                          NotSyncedCreatedContract {} -> do
                            [ tdInstant createdAt
                            , tdInstant $ updatedAt <|> createdAt
                            , tdContractId contractId Nothing []
                            , tdCentered [ DOOM.text $ intercalate ", " tags ]
                            , tdCentered [ buttonOutlinedInactive { label: DOOM.text "Syncing" } ]
                                -- ( [ DOM.div { className: "border border-dark rounded bg-white text-dark d-inline-block py-2 px-3 fw-bold" } do
                                --       DOOM.text "Syncing"
                                --   ] :: Array JSX
                                -- ) -- FIXME: Withdrawals should be still possible
                            ]
                          NotSyncedUpdatedContract { contractInfo } -> do
                            [ tdInstant createdAt
                            , tdInstant $ updatedAt <|> createdAt
                            , do
                                let
                                  ContractInfo { _runtime } = contractInfo
                                  transactionEndpoints = _runtime.transactions <#> \(_ /\ transactionEndpoint) -> transactionEndpoint
                                tdContractId contractId Nothing transactionEndpoints
                            , tdCentered [ DOOM.text $ intercalate ", " tags ]
                            , tdCentered
                                ( [ DOM.div { className: "border border-dark rounded bg-white text-dark d-inline-block py-2 px-3 fw-bold" } do
                                      DOOM.text "Syncing"
                                  ] :: Array JSX
                                ) -- FIXME: Withdrawals should be still possible
                            ]
          ]
        _, _ -> mempty

assetToString :: Cardano.AssetId -> Maybe String
assetToString Cardano.AdaAssetId = Nothing
assetToString (Cardano.AssetId _ assetName) = Cardano.assetNameToString assetName

prettyState :: V1.State -> String
prettyState = stringify <<< encodeJson

instantFromMillis :: Number -> Maybe Instant
instantFromMillis ms = instant (Duration.Milliseconds ms)

