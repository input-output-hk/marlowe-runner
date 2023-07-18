module Component.ContractList where

import Prelude

import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs as ApplyInputs
import Component.ApplyInputs.Machine as ApplyInputs.Machine
import Component.BodyLayout as BodyLayout
import Component.ContractDetails as ContractDetails
import Component.ContractTemplates.ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import Component.ContractTemplates.Escrow as Escrow
import Component.ContractTemplates.Swap as Swap
import Component.CreateContract (runLiteTag)
import Component.CreateContract as CreateContract
import Component.Types (ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, Slotting(..), WalletInfo)
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (buttonWithIcon, linkWithIcon)
import Component.Withdrawals as Withdrawals
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
import Data.Array (null)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either, hush)
import Data.Foldable (any, fold, or)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Function (on)
import Data.JSDate (fromDateTime) as JSDate
import Data.List (concat, intercalate)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (un)
import Data.Set as Set
import Data.String (contains, length)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration as Duration
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Types (ContractHeader(ContractHeader), Payout(..), PutTransactionRequest(..), Runtime(..), ServerURL, SlotNumber(..), Tags(..), TransactionEndpoint, TransactionsEndpoint, TxOutRef, WithdrawalsEndpoint, toTextEnvelope, txOutRefToString)
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Validator (liftFnM)
import React.Basic (fragment)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, readRef, useContext, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap (overlayTrigger, tooltip)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Table (striped) as Table
import ReactBootstrap.Table (table)
import ReactBootstrap.Types (placement)
import ReactBootstrap.Types as OverlayTrigger
import Utils.React.Basic.Hooks (useMaybeValue', useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))

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

type ContractListState = { modalAction :: Maybe ModalAction }

type Props =
  { possibleContracts :: Maybe (Array ContractInfo) -- `Maybe` indicates if the contracts where fetched already
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
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
  = NewContract
  | ContractDetails
    { contract :: Maybe V1.Contract
    , state :: Maybe V1.State
    , initialContract :: V1.Contract
    , initialState :: V1.State
    , transactionEndpoints :: Array Runtime.TransactionEndpoint
    }
  | ApplyInputs TransactionsEndpoint ApplyInputs.Machine.MarloweContext
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
    , sizing: Just FormControlLg
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

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  MessageHub msgHubProps <- asks _.msgHub
  Runtime runtime <- asks _.runtime
  walletInfoCtx <- asks _.walletInfoCtx
  slotting <- asks _.slotting

  createContractComponent <- CreateContract.mkComponent
  applyInputsComponent <- ApplyInputs.mkComponent
  withdrawalsComponent <- Withdrawals.mkComponent
  contractDetails <- ContractDetails.mkComponent
  escrowComponent <- Escrow.mkComponent
  swapComponent <- Swap.mkComponent
  contractForDifferencesWithOracleComponent <- ContractForDifferencesWithOracle.mkComponent

  liftEffect $ component "ContractList" \{ connectedWallet, possibleContracts } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)

    possibleModalAction /\ setModalAction /\ resetModalAction <- useMaybeValue'
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
      possibleContracts' = do
        contracts <- possibleContracts
        let
          -- Quick and dirty hack to display just submited contracts as first
          someFutureBlockNumber = Runtime.BlockNumber 9058430
          sortedContracts = case ordering.orderBy of
            OrderByCreationDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.createdAt)) contracts
            OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contracts
        pure $
          if ordering.orderAsc then sortedContracts
          else Array.reverse sortedContracts
      possibleContracts'' = do
        let
          filtered = do
            queryValue <- possibleQueryValue
            contracts <- possibleContracts'
            pure $ contracts # Array.filter \(ContractInfo { contractId, tags: contractTags }) -> do
              let
                tagList = runLiteTags contractTags
                pattern = Pattern queryValue
              contains pattern (txOutRefToString contractId) || or (map (contains pattern) tagList)
        filtered <|> possibleContracts'

      --         pure $ if ordering.orderAsc
      --           then sortedContracts
      --           else Array.reverse sortedContracts

      -- isLoadingContracts :: Boolean
      -- isLoadingContracts = case possibleContracts'' of
      --   Nothing -> true
      --   Just [] -> true
      --   Just contracts -> any (\(ContractInfo { marloweInfo }) -> isNothing marloweInfo) contracts

    pure $
      case possibleModalAction, connectedWallet of
        Just NewContract, Just cw -> createContractComponent
          { connectedWallet: cw
          , onDismiss: resetModalAction
          , onSuccess: \_ -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain."
                , "Contract status should change to 'Confirmed' at that point."
                ]
              resetModalAction
          }
        Just (ApplyInputs transactionsEndpoint marloweContext), Just cw -> do
          let
            onSuccess = \_ -> do
              msgHubProps.add $ Success $ DOOM.text $ fold
                [ "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." ]
              resetModalAction
          applyInputsComponent
            { transactionsEndpoint
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
        Just (ContractDetails { contract, state, initialContract, initialState, transactionEndpoints}), _ -> do
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

        Nothing, _ -> BodyLayout.component
          { title: "Your Marlowe Contracts"
          , description: DOOM.div_
              [ DOM.div { className: "pb-3" } $ DOM.p { className: "white-color h5" } $ DOOM.text "To the right, you will find a list of all contracts that your wallet is involved in on the Cardano Blockchain's `preview` network."
              , DOM.div { className: "pb-3" } $ DOM.p { className: "white-color h5" } $ DOOM.text "Your involvement means that one of your wallet addresses is a part of the contract (some contracts are non fully public) or that you have a token (so called \"role token\") which gives you permission to act as a party in some contract."
              -- , DOM.div "You can filter the list by contract ID or by contract creator. You can also click on a contract id to view its details."
              , DOM.div { className: "pb-3" } $ DOM.p { className: "white-color h5" } $ DOOM.text "Click on the 'New Contract' button to upload a new contract or try out one of our contract templates."
              ]
          , content: React.fragment
              [ DOM.div { className: "row p-4" } do
                  let
                    disabled = isNothing connectedWallet
                    newContractButton = buttonWithIcon
                      { icon: unsafeIcon "file-earmark-plus h5 me-1"
                      , label: DOOM.text "Create Contract"
                      , extraClassNames: "font-weight-bold me-2"
                      , disabled
                      , onClick: do
                          readRef possibleModalActionRef >>= case _ of
                            Nothing -> setModalAction NewContract
                            _ -> pure unit
                      }
                    templateContractButton = dropdownButton
                      { className: "d-inline-block"
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
              , case possibleContracts'' of
                  Just contracts | not (null contracts) -> DOM.div { className: "row" } $ DOM.div { className: "col-12 mt-3" } do
                    [ table { striped: Table.striped.boolean true, hover: true }
                        [ DOM.thead {} do
                            let
                              orderingTh = Table.orderingHeader ordering updateOrdering
                              th label = DOM.th { className: "text-center text-muted" } [ label ]
                            [ DOM.tr {}
                                [ do
                                    let
                                      label = DOOM.fragment [ DOOM.text "Created" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                                    orderingTh label OrderByCreationDate
                                , do
                                    let
                                      label = DOOM.fragment [ DOOM.text "Updated" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                                    orderingTh label OrderByLastUpdateDate
                                , DOM.th { className: "text-center w-16rem" } $ DOOM.text "Contract Id"
                                , th $ DOOM.text "Tags"
                                , th $ DOOM.text "Actions"
                                ]
                            ]
                        , DOM.tbody {} $ contracts <#> \ci@(ContractInfo { _runtime, endpoints, marloweInfo, tags: contractTags }) ->
                            let
                              ContractHeader { contractId } = _runtime.contractHeader
                              tdCentered = DOM.td { className: "text-center" }
                              tdSlotInfo Nothing = tdCentered $ []
                              tdSlotInfo (Just slotNo) = do
                                let
                                  slotNoInfo = do
                                    let
                                      dateTime = slotToTimestamp slotting slotNo
                                      jsDate = JSDate.fromDateTime dateTime
                                    [ DOOM.text $ JSDate.toLocaleDateString jsDate
                                    , DOOM.br {}
                                    , DOOM.text $ JSDate.toLocaleTimeString jsDate
                                    ]
                                DOM.td { className: "text-center" } $ DOM.small {} slotNoInfo

                            in
                              DOM.tr { className: "align-middle" }
                                [ tdSlotInfo $ _.slotNo <<< un Runtime.BlockHeader <$> ContractInfo.createdAt ci
                                , tdSlotInfo $ _.slotNo <<< un Runtime.BlockHeader <$> ContractInfo.updatedAt ci
                                , DOM.td { className: "text-center" } $ DOM.span { className: "d-flex" }
                                    [ DOM.a
                                        do
                                          let
                                            onClick = case marloweInfo of
                                              Just (MarloweInfo { state, currentContract, initialContract, initialState }) -> do
                                                let
                                                  transactionEndpoints = _runtime.transactions <#> \(_ /\ transactionEndpoint) -> transactionEndpoint
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
                                        [ text $ txOutRefToString contractId ]
                                    , DOM.a { href: "#", className: "cursor-pointer text-decoration-none text-decoration-underline-hover text-reset" } $
                                        Icons.toJSX $ unsafeIcon "clipboard-plus ms-1 d-inline-block"
                                    ]
                                , tdCentered
                                    [ do
                                        let
                                          tags = runLiteTags contractTags
                                        DOOM.text $ intercalate ", " tags
                                    ]
                                , tdCentered
                                    [ do
                                        case endpoints.transactions, marloweInfo of
                                          Just transactionsEndpoint, Just (MarloweInfo { initialContract, state: Just state, currentContract: Just contract }) -> do
                                            let
                                              marloweContext = { initialContract, state, contract }
                                            linkWithIcon
                                              { icon: unsafeIcon $ "fast-forward-fill" <> actionIconSizing
                                              , label: mempty
                                              , tooltipText: Just "Apply available inputs to the contract"
                                              , tooltipPlacement: Just placement.left
                                              , onClick: setModalAction $ ApplyInputs transactionsEndpoint marloweContext
                                              }
                                          _, Just (MarloweInfo { state: Nothing, currentContract: Nothing }) -> linkWithIcon
                                            { icon: unsafeIcon $ "file-earmark-check-fill success-color" <> actionIconSizing
                                            , tooltipText: Just "Contract is completed - click on contract id to see in Marlowe Explorer"
                                            , tooltipPlacement: Just placement.left
                                            , label: mempty
                                            , onClick: mempty
                                            }
                                          _, _ -> mempty
                                    , case marloweInfo, possibleWalletContext of
                                        Just (MarloweInfo { currencySymbol: Just currencySymbol, state: _, unclaimedPayouts }), Just { balance } -> do
                                          let
                                            balance' = Map.filterKeys (eq currencySymbol) balance
                                            roleTokens = List.toUnfoldable <<< concat <<< map Set.toUnfoldable <<< map Map.keys <<< Map.values $ balance'
                                          case Array.uncons (Array.intersect roleTokens (map (\(Payout { role }) -> role) unclaimedPayouts)) of
                                            Just { head, tail } ->
                                              linkWithIcon
                                                { icon: unsafeIcon $ "cash-coin warning-color" <> actionIconSizing
                                                , label: mempty
                                                , tooltipText: Just "This wallet has funds available for withdrawal from this contract. Click to submit a withdrawal"
                                                , onClick: setModalAction $ Withdrawal runtime.withdrawalsEndpoint (NonEmptyArray.cons' head tail) contractId
                                                }
                                            _ -> mempty
                                        _, _ -> mempty
                                    ]
                                ]
                        ]
                    ]
                  _ ->
                    DOM.div
                      -- { className: "col-12 position-absolute top-0 start-0 w-100 h-100 d-flex justify-content-center align-items-center blur-bg z-index-sticky"
                      { className: "col-12 position-relative d-flex justify-content-center align-items-center blur-bg z-index-sticky"
                      }
                      $ loadingSpinnerLogo
                          {}
              ]
          }
        _, _ -> mempty

prettyState :: V1.State -> String
prettyState = stringify <<< encodeJson

instantFromMillis :: Number -> Maybe Instant
instantFromMillis ms = instant (Duration.Milliseconds ms)

slotToTimestamp :: Slotting -> SlotNumber -> DateTime
slotToTimestamp (Slotting { slotZeroTime, slotLength }) (SlotNumber n) = fromMaybe bottom do
  let
    slotNo = BigInt.fromInt n
    time = BigInt.toNumber $ slotZeroTime + (slotNo * slotLength)
  instant <- instantFromMillis time
  pure $ Instant.toDateTime instant
