module Component.ContractList where

import Prelude

import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs as ApplyInputs
import Component.CreateContract as CreateContract
import Component.Types (ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, WalletInfo)
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (linkWithIcon)
import Contrib.Data.DateTime.Instant (millisecondsFromNow)
import Contrib.Fetch (FetchError)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson, stringify)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (fold, foldMap)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (un)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, TimeInterval(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Types (ContractHeader(ContractHeader), PutTransactionRequest(..), ServerURL, TransactionEndpoint, TransactionsEndpoint, TxOutRef, toTextEnvelope, txOutRefToString, txOutRefToUrlEncodedString)
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Validator (liftFnM)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (div_, span_, text, hr) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (Hook, JSX, UseState, component, readRef, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseForm (useForm)
import React.Basic.Hooks.UseForm as UseForm
import ReactBootstrap (overlayTrigger, tooltip)
import ReactBootstrap.FormBuilder (BootstrapForm, textInput)
import ReactBootstrap.FormBuilder as FormBuilder
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Table (striped) as Table
import ReactBootstrap.Table (table)
import ReactBootstrap.Types as OverlayTrigger
import Utils.React.Basic.Hooks (useMaybeValue', useStateRef')
import Wallet as Wallet

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

data ModalAction = NewContract | ApplyInputs TransactionsEndpoint V1.Contract V1.State TimeInterval

derive instance Eq ModalAction

queryFieldId :: FieldId
queryFieldId = FieldId "query"

mkForm :: (Maybe String -> Effect Unit) -> BootstrapForm Effect Query { query :: Maybe String }
mkForm onFieldValueChange = FormBuilder.evalBuilder' ado
  query <- textInput
    { validator: liftFnM \value -> do
      onFieldValueChange value -- :: Batteries.Validator Effect _ _  _
      pure value
    , name: Just queryFieldId
    , placeholder: "Filter contracts..."
    }
  in
    { query }

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  MessageHub msgHubProps <- asks _.msgHub

  createContractComponent <- CreateContract.mkComponent
  applyInputsComponent <- ApplyInputs.mkComponent

  liftEffect $ component "ContractList" \{ connectedWallet, contractList } -> React.do
    possibleModalAction /\ setModalAction /\ resetModalAction <- useMaybeValue'
    possibleModalActionRef <- useStateRef' possibleModalAction
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }
    possibleQueryValue /\ setQueryValue <- useState' Nothing
    let
      form = mkForm setQueryValue
    { formState } <- useForm
      { spec: form
      , onSubmit: const $ pure unit
      , validationDebounce: Seconds 0.5
      }

    let
      contractList' = do
        let
          -- Quick and dirty hack to display just submited contracts as first
          someFutureBlockNumber = Runtime.BlockNumber 9058430
          sortedContracts = case ordering.orderBy of
            OrderByCreationDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.createdAt)) contractList
            OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contractList
        if ordering.orderAsc then sortedContracts
        else Array.reverse sortedContracts
      contractList'' = case possibleQueryValue of
        Nothing -> contractList'
        Just queryValue ->
          Array.filter (\(ContractInfo { contractId }) -> contains (Pattern queryValue) (txOutRefToString contractId)) contractList'
    pure $ do
      DOOM.div_
        [ case possibleModalAction, connectedWallet of
            Just NewContract, Just cw -> createContractComponent
              { connectedWallet: cw
              , onDismiss: resetModalAction
              , onSuccess: \_ -> do
                  msgHubProps.add $ Success $ DOOM.text $ fold
                    [ "Successfully created and submitted the contract. Contract transaction awaits to be included in the blockchain."
                    , "Contract status should change to 'Confirmed' at that point."
                    ]
                  resetModalAction
              , inModal: true
              }
            Just (ApplyInputs transactionsEndpoint contract st timeInterval), Just cw -> do
              let
                onSuccess = \_ -> do
                  msgHubProps.add $ Success $ DOOM.text $ fold
                    [ "Successfully applied the inputs. Input application transaction awaits to be included in the blockchain." ]
                  resetModalAction
              applyInputsComponent
                { inModal: true
                , transactionsEndpoint
                , timeInterval
                , contract
                , state: st
                , connectedWallet: cw
                , onSuccess
                , onDismiss: resetModalAction
                }
            _, _ -> mempty
        , DOM.div { className: "row justify-content-end" } do
            let
              disabled = isNothing connectedWallet
              addContractLink = linkWithIcon
                { icon: Icons.fileEarmarkPlus
                , label: DOOM.text "Add contract"
                , disabled
                , onClick: do
                    readRef possibleModalActionRef >>= case _ of
                      Nothing -> setModalAction NewContract
                      _ -> pure unit
                }
              fields = UseForm.renderForm form formState
              body = DOM.div { className: "form-group" } fields
                -- actions = DOOM.fragment []
            [ DOM.div { className: "col-9 text-end" } $
              [ body
              -- , actions
              ]
            , DOM.div { className: "col-3 text-end" } $ Array.singleton $
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
            ]
        , DOOM.hr {}
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
                              tooltipJSX = tooltip {} (-- DOOM.text $
                                            case marloweInfo of
                                                Just (MarloweInfo {currentContract: Just contract, state: Just contractState}) ->
                                                  [DOM.div {} [text $ show contract], DOOM.hr {}, DOM.div {} [text $ prettyState contractState] ]
                                                _ -> mempty)
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
                                      setModalAction $ ApplyInputs transactionsEndpoint currentContract currentState interval
                                  }
                                Just _, Nothing -> DOOM.text "No Marlowe info"
                                Nothing, _ -> DOOM.text "No transactions endpoint"
                                _, _ -> DOOM.text ""
                            ]
                        ]
                )
                contractList''
            ]
        ]

prettyState :: V1.State -> String
prettyState = stringify <<< encodeJson
