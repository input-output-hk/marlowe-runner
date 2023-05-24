module Component.ContractList where

import Prelude

import Actus.Domain (CashFlow)
import Actus.Domain.ContractTerms (ContractTerms)
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs as ApplyInputs
import Component.CreateContract as CreateContract
import Component.Modal (mkModal)
import Component.Types (ActusContractId(..), ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, WalletInfo(..))
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (link, linkWithIcon)
import Contrib.Data.DateTime.Instant (millisecondsFromNow)
import Contrib.Fetch (FetchError)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, intInput, textInput)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Table (striped) as Table
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Types as OverlayTrigger
import Control.Monad.Reader.Class (asks)
import Data.Array (elem, singleton, toUnfoldable)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.DateTime.Instant (unInstant)
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.FormURLEncoded.Query (FieldId(..), Query(..))
import Data.Function (on)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (un, unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Time.Duration as Duration
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now, nowDateTime)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types (Case(..), Contract(..), Input(..), InputContent(..), Party, TimeInterval(..), Token)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), InputContent(..), Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web.Client (post')
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Streaming (TxHeaderWithEndpoint)
import Marlowe.Runtime.Web.Types (ContractHeader(..), Metadata, PostTransactionsRequest(..), TxHeader(..), TxOutRef, txOutRefToString, txOutRefToUrlEncodedString)
import Marlowe.Runtime.Web.Types (PostMerkleizationRequest(..), PostMerkleizationResponse(..), PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), Runtime(..), ServerURL, TextEnvelope(..), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Marlowe.Runtime.Web.Types as Runtime
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Batteries as Batteries
import React.Basic (fragment) as DOOM
import React.Basic.DOM (div_, span_, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, readRef, useState, (/\))
import React.Basic.Hooks (JSX, component, useContext, useState, (/\))
import React.Basic.Hooks as React
import Utils.React.Basic.Hooks (useStateRef, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..), walletAddresses)

type ContractId = TxOutRef

type ProjectedCashFlows = List (CashFlow Decimal Party)

type ValidationError = String

data FormState
  = NotValidated
  | Failure ValidationError
  | Validated (ContractTerms /\ Contract)

-- An example of a simple "custom hook"
useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

type SubmissionError = String

type ContractListState =
  { modalAction :: Maybe ModalAction
  , metadata :: Maybe Metadata
  }

type Props =
  { contractList :: Array ContractInfo
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

onApplyInputs runtime cardanoMultiplatformLib possibleWalletContext { transactionsEndpoint, marloweInfo } { party, token, value } cw = do
  now <- nowDateTime
  -- FIXME: move aff flow into `useAff` on the component level
  launchAff_ $ do
    case possibleWalletContext of
      Just { changeAddress: Just changeAddress } -> do
        let WalletInfo { wallet: walletApi } = cw
        addresses <- walletAddresses cardanoMultiplatformLib walletApi

        let
          inputs = singleton $ NormalInput (IDeposit party party token value)

          invalidBefore = fromMaybe now $ adjust (Duration.Minutes (-2.0)) now
          invalidHereafter = fromMaybe now $ adjust (Duration.Minutes 2.0) now
          collateralUTxOs = []

          req = PostTransactionsRequest
            { inputs
            , invalidBefore
            , invalidHereafter
            , metadata: mempty
            , tags: mempty
            , changeAddress
            , addresses
            , collateralUTxOs
            }

        post' runtime.serverURL transactionsEndpoint req
          >>= case _ of
            Right ({ resource: PostTransactionsResponse postTransactionsResponse, links: { transaction: transactionEndpoint } }) -> do
              traceM postTransactionsResponse
              let
                { tx } = postTransactionsResponse
                TextEnvelope { cborHex: txCborHex } = tx
              Wallet.signTx walletApi txCborHex true >>= case _ of
                Right witnessSet -> do
                  submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                    Right _ -> do
                      traceM "Successfully submitted the transaction"
                      -- liftEffect $ msgHubProps.add $ Success $ DOOM.text $ "Successfully submitted a transaction"
                    -- liftEffect $ onSuccess contractEndpoint
                    Left err -> do
                      traceM "Error while submitting the transaction"
                      -- liftEffect $ msgHubProps.add $ Error $ DOOM.text $ "Error while submitting the transaction"
                      traceM err

                Left err -> do
                  traceM err
                  pure unit

              pure unit
            Left _ -> do
              traceM token
              -- traceM $ BigInt.toString value
              traceM "error"
              pure unit

        pure unit
      _ -> do
        -- Note: this happens, when the contract is in status `Unsigned`
        pure unit

-- updateState _ { newInput = Nothing }

data ModalAction = NewContract | ApplyInputs TransactionsEndpoint V1.Contract V1.State TimeInterval

derive instance Eq ModalAction

queryFieldId = FieldId "query"

form :: BootstrapForm Effect Query _
form = FormBuilder.evalBuilder' ado
  -- amount <- intInput {}
  query <- textInput { validator: identity :: Batteries.Validator Effect _ _  _, name: Just queryFieldId }
  -- token <- textInput { validator: identity }
  in
    { query
    -- , party
    -- , token
    }

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect $ mkModal
  MessageHub msgHubProps <- asks _.msgHub

  createContractComponent <- CreateContract.mkComponent
  applyInputsComponent <- ApplyInputs.mkComponent
  walletInfoCtx <- asks _.walletInfoCtx
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "ContractList" \{ connectedWallet, contractList } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    ((state :: ContractListState) /\ updateState) <- useState { modalAction: Nothing, metadata: Nothing }
    stateRef <- useStateRef' state
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }

    let
      onSubmit :: { result :: _ , payload :: _ } -> Effect Unit
      onSubmit { result } = do
        traceM result
        pure unit

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    let
      queryValue = case Map.lookup queryFieldId formState.fields of
        Just { value: [ value ]} -> value
        _ -> ""
      contractList' = do
        let
          -- Quick and dirty hack to display just submited contracts as first
          someFutureBlockNumber = Runtime.BlockNumber 9058430
          sortedContracts = case ordering.orderBy of
            OrderByCreationDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.createdAt)) contractList
            OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contractList
        if ordering.orderAsc then sortedContracts
        else Array.reverse sortedContracts

    pure $ do
      DOOM.div_
        [ case state.modalAction, connectedWallet of
            Just NewContract, Just cw -> createContractComponent
              { connectedWallet: cw
              , onDismiss: updateState _ { modalAction = Nothing }
              , onSuccess: \_ -> do
                  msgHubProps.add $ Success $ DOOM.text $ fold
                    [ "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain."
                    , "Contract status should change to 'Confirmed' at that point."
                    ]
                  updateState _ { modalAction = Nothing }
              , inModal: true
              }
            Just (ApplyInputs transactionsEndpoint contract state timeInterval), Just cw -> do
              let
                onSuccess = \_ -> do
                  msgHubProps.add $ Success $ DOOM.text $ fold
                    [ "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." ]
                  updateState _ { modalAction = Nothing }
              applyInputsComponent
                { inModal: true
                , transactionsEndpoint
                , timeInterval
                , contract
                , state
                , connectedWallet: cw
                , onSuccess
                , onDismiss: updateState _ { modalAction = Nothing }
                }
            _, _ -> mempty
        , DOM.div { className: "row justify-content-end" } $ Array.singleton $ do
            let
              disabled = isNothing connectedWallet
              addContractLink = linkWithIcon
                { icon: Icons.fileEarmarkPlus
                , label: DOOM.text "Add contract"
                , disabled
                , onClick: do
                    readRef stateRef >>= _.modalAction >>> case _ of
                      Nothing -> updateState _ { modalAction = Just NewContract }
                      _ -> pure unit
                }
            DOM.div { className: "col-3 text-end" } $ Array.singleton $
              if disabled then do
                let
                  tooltipJSX = tooltip {} (DOOM.text "Connect to a wallet to add a contract")
                overlayTrigger
                  { overlay: tooltipJSX
                  , placement: OverlayTrigger.placement.bottom
                  }
                  -- Disabled button doesn't trigger the hook,
                  -- so we wrap it in a `span`
                  (DOOM.span_ [ addContractLink ])
              else
                addContractLink
        , DOM.div { className: "row" } do
            let
              fields = UseForm.renderForm form formState
              body = DOM.div { className: "form-group" } fields
              actions = DOOM.fragment
                [ DOM.button
                    do
                      let
                        disabled = case result of
                          Just (V (Right _) /\ _) -> false
                          _ -> true
                      { className: "btn btn-primary"
                      , onClick: onSubmit'
                      , disabled
                      }
                    [ R.text "Submit" ]
                ]
            [ body
            , actions
            ]

        , DOM.div { className: "row" } $ Array.singleton $ case state.metadata of
            Just (metadata) -> modal $
              { body: text $ maybe "Empty Metadata" (show <<< _.contractTerms <<< unwrap) $ M.decodeMetadata metadata -- TODO: encode contractTerms as JSON
              , onDismiss: updateState _ { metadata = Nothing }
              , title: text "Contract Terms"
              , footer: text ""
              }
            Nothing -> mempty
        , table { striped: Table.striped.boolean true, hover: true }
            [ DOM.thead {} do
                let
                  orderingTh = Table.orderingHeader ordering updateOrdering
                  th label = DOM.th { className: "text-center text-muted" } [ label ]
                [ DOM.tr {}
                    [ do
                        let
                          label = DOOM.fragment [ DOOM.text "Created" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                        orderingTh label OrderByCreationDate
                    , th $ DOOM.text "Contract Id"
                    , th $ DOOM.text "Status"
                    , th $ DOOM.text "Inputs"
                    ]
                ]
            , DOM.tbody {} $ map
                ( \ci@(ContractInfo { _runtime, endpoints, marloweInfo }) ->
                    let
                      ContractHeader { contractId, status } = _runtime.contractHeader
                      tdCentered = DOM.td { className: "text-center" }
                    in
                      DOM.tr {}
                        [ tdCentered [ text $ foldMap show $ map (un Runtime.BlockNumber <<< _.blockNo <<< un Runtime.BlockHeader) $ ContractInfo.createdAt ci ]
                        , tdCentered [ DOM.a
                           { className: "btn btn-link text-decoration-none text-reset text-decoration-underline-hover"
                           , target: "_blank"
                           , href: "http://marlowe.palas87.es:8002/contractView?tab=info&contractId=" <> (txOutRefToUrlEncodedString contractId)
                           }
                           [ text $ txOutRefToString contractId ]
                          ]
                        , DOM.td { className: "text-center" } $ do
                            let
                              tooltipJSX = tooltip {} (DOOM.text $
                                            case marloweInfo of
                                                Just (MarloweInfo {currentContract: Just contract}) -> show contract
                                                _ -> "")
                            overlayTrigger
                              { overlay: tooltipJSX
                              , placement: OverlayTrigger.placement.bottom
                              } $ DOM.span {} [ show status ]
                        , tdCentered
                            [ case endpoints.transactions, marloweInfo of
                                Just transactionsEndpoint, Just (MarloweInfo { state: Just currentState, currentContract: Just currentContract }) -> linkWithIcon
                                  { icon: Icons.listOl
                                  , label: DOOM.text "Apply"
                                  , onClick: do
                                      invalidBefore <- millisecondsFromNow (Milliseconds (Int.toNumber $ (-5) * 60 * 1000))
                                      invalidHereafter <- millisecondsFromNow (Milliseconds (Int.toNumber $ 5 * 60 * 1000))
                                      let
                                        interval = TimeInterval invalidBefore invalidHereafter

                                      updateState _ { modalAction = Just (ApplyInputs transactionsEndpoint currentContract currentState interval) }
                                  }
                                Just transactionEndpoint, Nothing -> DOOM.text "No Marlowe info"
                                Nothing, _ -> DOOM.text "No transactions endpoint"
                                _, _ -> DOOM.text "Awaiting details"
                            ]
                        ]
                )
                contractList'
            ]
        ]
