module Component.ApplyInputs where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs.Forms (mkApplyInputForm)
import Component.ApplyInputs.Machine (AutoRun(..), InputChoices(..), mkEnvironment)
import Component.ApplyInputs.Machine as Machine
import Component.BodyLayout (descriptionLink, wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.InputHelper (ChoiceInput, DepositInput(..), ExecutionPath, NotifyInput, StartPathSelection(..), toIDeposit)
import Component.InputHelper as InputHelper
import Component.Types (ContractInfo, MkComponentM, WalletInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widgets (SpinnerOverlayHeight(..), backToContractListLink, link, marlowePreview, marloweStatePreview, spinnerOverlay)
import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder')
import Contrib.Polyform.FormSpecs.StatefulFormSpec as StatefulFormSpec
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.React.Basic.Hooks.UseMooreMachine (MooreMachineSpec, useMooreMachine)
import Contrib.React.Basic.Hooks.UseMooreMachine as Moore
import Contrib.React.Basic.Hooks.UseStatefulFormSpec (useStatefulFormSpec)
import Contrib.React.MarloweGraph (marloweGraph)
import Contrib.React.Svg (loadingSpinnerLogo)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), FieldLayout(..), LabelSpacing(..), booleanField, choiceField, radioFieldChoice)
import Contrib.ReactSyntaxHighlighter (jsonSyntaxHighlighter)
import Control.Monad.Reader.Class (asks)
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt.Argonaut (toString)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (instant, unInstant)
import Data.Decimal as Decimal
import Data.Either (Either(..), hush)
import Data.Foldable (foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics (computeTransaction) as V1
import Language.Marlowe.Core.V1.Semantics.Types (Action(..), Ada(..), Case(..), Contract(..), Environment(..), Input(..), InputContent(..), Party(..), State, TimeInterval, Token(..), TransactionInput(..), TransactionOutput(..), Value(..)) as V1
import Language.Marlowe.Core.V1.Semantics.Types (Input(..))
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent, PostTransactionsRequest(PostTransactionsRequest), PostTransactionsResponse, PutTransactionRequest(PutTransactionRequest), ServerURL, TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnMaybe)
import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useEffect, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)
import Wallet as Wallet

type Result = V1.Contract

data ContractData = ContractData
  { contract :: V1.Contract
  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  }

type ClientError' = ClientError String

create :: ContractData -> ServerURL -> ContractsEndpoint -> Aff (Either ClientError' { resource :: PostContractsResponseContent, links :: { contract :: ContractEndpoint } })
create contractData serverUrl contractsEndpoint = do
  let
    ContractData { contract, changeAddress, usedAddresses } = contractData
    req = PostContractsRequest
      { metadata: mempty
      , roles: Nothing
      , tags: mempty
      , contract
      , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
      , changeAddress: changeAddress
      , addresses: usedAddresses <> [ changeAddress ]
      , collateralUTxOs: []
      }

  post' serverUrl contractsEndpoint req

submit :: CborHex TransactionWitnessSetObject -> ServerURL -> TransactionEndpoint -> Aff (Either FetchError Unit)
submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutTransactionRequest textEnvelope
  put' serverUrl contractEndpoint req

contractSection :: V1.Contract -> V1.State -> Maybe ExecutionPath -> JSX
contractSection contract state possibleExecutionPath = do
  tabs { fill: false, justify: false, defaultActiveKey: "graph", variant: Tabs.variant.pills } do
    let
      renderTab props children = tab props $ DOM.div { className: "mt-4 h-vh50 d-flex align-items-stretch" } children
    [ tab
        { eventKey: eventKey "graph"
        , title: DOOM.span_
            -- [ Icons.toJSX $ unsafeIcon "diagram-2"
            [ DOOM.text " Source graph"
            ]
        }
        $ DOM.div { className: "mt-4 h-vh50 d-flex align-items-stretch border-1 border rounded p-1" }
        $ marloweGraph
            { contract: contract
            , executionPath: NoProblem.fromMaybe possibleExecutionPath
            }
    , renderTab
        { eventKey: eventKey "source"
        , title: DOOM.span_
            -- [ Icons.toJSX $ unsafeIcon "filetype-yml"
            [ DOOM.text " Source code"
            ]
        }
        $ marlowePreview contract
    , renderTab
        { eventKey: eventKey "state"
        , title: DOOM.span_
            -- [ Icons.toJSX $ unsafeIcon "bank"
            [ DOOM.text " Contract state"
            ]
        }
        $ marloweStatePreview state
    ]

type DepositFormComponentProps =
  { depositInputs :: NonEmptyArray DepositInput
  , connectedWallet :: WalletInfo Wallet.Api
  , marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSubmit :: V1.Input -> Effect Unit
  }

mkDepositFormComponent :: MkComponentM (DepositFormComponentProps -> JSX)
mkDepositFormComponent = do
  liftEffect $ component "ApplyInputs.DepositFormComponent" \props@{ depositInputs, onDismiss, marloweContext } -> React.do
    submitting /\ setSubmitting <- useState' false
    let
      choices = RadioButtonFieldChoices do
        let
          toChoice idx (DepositInput _ _ token value _) = do
            let
              label = "Deposit " <> case token of
                V1.Token "" "" -> do
                  let
                    possibleDecimal = do
                      million <- Decimal.fromString "1000000"
                      lovelace <- Decimal.fromString $ BigInt.toString value
                      pure $ lovelace / million
                  case possibleDecimal of
                    Just value' -> Decimal.toString value' <> " ₳"
                    Nothing -> toString value <> " of Lovelace"
                V1.Token currencySymbol name -> toString value <> " of " <> " currency " <> currencySymbol <> " of token " <> name
            radioFieldChoice (show idx) (DOOM.text label)
        { switch: true
        , choices: ArrayAL.fromNonEmptyArray $ mapWithIndex toChoice depositInputs
        }

      validator :: Batteries.Validator Effect _ _ _
      validator = do
        let
          value2Deposit = Map.fromFoldable $ mapWithIndexFlipped depositInputs \idx deposit -> show idx /\ deposit
        liftFnMaybe (\v -> [ "Invalid choice: " <> show v ]) \possibleIdx -> do
          idx <- possibleIdx
          Map.lookup idx value2Deposit

      formSpec = evalBuilder' $
        choiceField { choices, validator, showValidity: false, initial: "0", touched: true }

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right deposit) /\ _) -> case toIDeposit deposit of
          Just ideposit -> props.onSubmit $ NormalInput ideposit
          Nothing -> pure unit
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.0
      }

    possibleTimeInterval /\ setPossibleTimeInterval <- useState' Nothing
    useEffect result do
      V1.Environment { timeInterval } <- mkEnvironment
      setPossibleTimeInterval $ Just timeInterval
      pure $ pure unit

    let
      possibleExecutionPath = case result, possibleTimeInterval of
        Just (V (Right deposit) /\ _), Just ti ->
          join $ hush $ InputHelper.executionPath [ toIDeposit deposit /\ ti /\ StartPathSelection (not submitting) ] marloweContext.contract marloweContext.state
        _, _ -> Nothing

    pure do
      let
        fields = StatelessFormSpec.renderFormSpec formSpec formState
        body = fragment $
          [ contractSection marloweContext.contract marloweContext.state possibleExecutionPath
          , DOOM.hr {}
          ] <> [ DOM.div { className: "form-group" } fields ]
        actions = fragment
          [ DOM.div { className: "row" } $
              [ DOM.div { className: "col-12" } $
                  [ DOM.button
                      do
                        let
                          disabled = case result of
                            Just (V (Right _) /\ _) -> false
                            _ -> true
                        { className: "btn btn-primary w-100"
                        , onClick: onSubmit'
                        , disabled
                        , onMouseOver: handler_ $ setSubmitting true
                        , onMouseOut: handler_ $ setSubmitting false
                        }
                      [ R.text "Make deposit"
                      , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                      ]
                  ]
              , backToContractListLink onDismiss
              ]
          ]
      wrappedContentWithFooter body actions

type ChoiceFormComponentProps =
  { choiceInputs :: NonEmptyArray ChoiceInput
  , connectedWallet :: WalletInfo Wallet.Api
  , marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSubmit :: V1.Input -> Effect Unit
  }

mkChoiceFormComponent :: MkComponentM (ChoiceFormComponentProps -> JSX)
mkChoiceFormComponent = do
  liftEffect $ component "ApplyInputs.DepositFormComponent" \props@{ choiceInputs, marloweContext, onDismiss } -> React.do
    submitting /\ setSubmitting <- React.useState' false
    let
      formSpec = mkApplyInputForm choiceInputs

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right ichoice) /\ _) -> props.onSubmit $ NormalInput ichoice
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatefulFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.0
      , state: Nothing
      }

    -- FIXME: we don't detect or handle timeouts here.
    -- We should accept timeout actually as a parameter and guard for that across
    -- all the input application components.
    possibleTimeInterval /\ setPossibleTimeInterval <- useState' Nothing
    useEffect result do
      V1.Environment { timeInterval } <- mkEnvironment
      setPossibleTimeInterval $ Just timeInterval
      pure $ pure unit

    let
      fields = StatefulFormSpec.renderFormSpec formSpec formState
      possibleExecutionPath = case result, possibleTimeInterval of
        Just (V (Right ichoice) /\ _), Just ti ->
          join $ hush $ InputHelper.executionPath [ Just ichoice /\ ti /\ StartPathSelection (not submitting) ] marloweContext.contract marloweContext.state
        _, _ -> Nothing
      body = DOOM.div_ $
        [ contractSection marloweContext.contract marloweContext.state possibleExecutionPath
        , DOOM.hr {}
        ] <> [ DOM.div { className: "form-group" } fields ]
      actions = fragment
        [ DOM.div { className: "row" } $
            [ DOM.div { className: "col-12" } $
                [ DOM.button
                    do
                      let
                        disabled = case result of
                          Just (V (Right _) /\ _) -> false
                          _ -> true
                      { className: "btn btn-primary w-100"
                      , onClick: onSubmit'
                      , disabled
                      , onMouseOver: handler_ $ setSubmitting true
                      , onMouseOut: handler_ $ setSubmitting false
                      }
                    [ R.text "Advance contract"
                    , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                    ]
                ]
            , backToContractListLink onDismiss
            ]
        ]
    pure $ wrappedContentWithFooter body actions

type NotifyFormComponentProps =
  { notifyInput :: NotifyInput
  , connectedWallet :: WalletInfo Wallet.Api
  , marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSubmit :: Effect Unit
  , timeInterval :: V1.TimeInterval
  }

mkNotifyFormComponent :: MkComponentM (NotifyFormComponentProps -> JSX)
mkNotifyFormComponent = do
  liftEffect $ component "ApplyInputs.NotifyFormComponent" \{ marloweContext, onDismiss, onSubmit, timeInterval } -> React.do
    submitting /\ setSubmitting <- useState' false
    pure do
      let
        possibleExecutionPath = join $ hush $ InputHelper.executionPath
          [ Just V1.INotify /\ timeInterval /\ StartPathSelection (not submitting) ]
          marloweContext.contract
          marloweContext.state
        body = DOOM.div_ $
          [ contractSection marloweContext.contract marloweContext.state possibleExecutionPath
          , DOOM.hr {}
          ]
        actions = fragment
          [ DOM.div { className: "row" } $
              [ DOM.div { className: "col-12" } $
                  [ DOM.button
                      do
                        { className: "btn btn-primary w-100"
                        , onClick: handler_ onSubmit
                        , onMouseOver: handler_ $ setSubmitting true
                        , onMouseOut: handler_ $ setSubmitting false
                        }
                      [ R.text "Advance contract"
                      , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                      ]
                  ]
              , backToContractListLink onDismiss
              ]
          ]
      fragment
        [ DOM.div { className: "p-3" } body
        , DOM.div { className: "p-3 mt-auto" } actions
        ]

type AdvanceFormComponentProps =
  { marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSubmit :: Effect Unit
  , timeInterval :: V1.TimeInterval
  }

mkAdvanceFormComponent :: MkComponentM (AdvanceFormComponentProps -> JSX)
mkAdvanceFormComponent = do
  liftEffect $ component "ApplyInputs.AdvanceFormComponent" \{ marloweContext, onDismiss, onSubmit, timeInterval } -> React.do
    submitting /\ setSubmitting <- useState' false
    let
      possibleExecutionPath = join $ hush $ InputHelper.executionPath
        [ Nothing /\ timeInterval /\ StartPathSelection (not submitting) ]
        marloweContext.contract
        marloweContext.state

      body = DOOM.div_ $
        [ contractSection marloweContext.contract marloweContext.state possibleExecutionPath
        ]
      actions = fragment
        [ DOM.div { className: "row" } $
            [ DOM.div { className: "col-12" } $
                [ DOM.button
                    do
                      { className: "btn btn-primary w-100"
                      , onClick: handler_ onSubmit
                      , onMouseOver: handler_ $ setSubmitting true
                      , onMouseOut: handler_ $ setSubmitting false
                      }
                    [ R.text "Advance contract"
                    , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                    ]
                ]
            , backToContractListLink onDismiss
            ]
        ]
    pure $ fragment
      [ DOM.div { className: "p-3" } body
      , DOM.div { className: "p-3 mt-auto" } actions
      ]

data CreateInputStep
  = SelectingInputType
  | PerformingDeposit (NonEmptyArray DepositInput)
  | PerformingNotify (NonEmptyArray NotifyInput)
  | PerformingChoice (NonEmptyArray ChoiceInput)
  | PerformingAdvance V1.Contract

data Step = Creating CreateInputStep

mkMachineSpec
  :: Machine.MarloweContext
  -> TransactionsEndpoint
  -> WalletInfo Wallet.Api
  -> CardanoMultiplatformLib.Lib
  -> Moore.OnStateTransition Machine.State
  -> Runtime
  -> MooreMachineSpec Machine.State Machine.Action Machine.State
mkMachineSpec marloweContext transactionsEndpoint connectedWallet cardanoMultiplatformLib onStateTransition runtime = do
  let
    env = { connectedWallet, cardanoMultiplatformLib, runtime }

  { initialState: Machine.initialState marloweContext transactionsEndpoint (AutoRun true)
  , step: Machine.step
  , driver: Machine.driver env
  , output: identity
  , onStateTransition
  }

type ContractDetailsProps =
  { marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSuccess :: AutoRun -> Effect Unit
  }

mkContractDetailsComponent :: MkComponentM (ContractDetailsProps -> JSX)
mkContractDetailsComponent = do
  let
    autoRunFormSpec = evalBuilder' $ AutoRun <$> booleanField
      { label: DOOM.text "Auto run"
      , layout: MultiColumn { sm: Col3Label, md: Col2Label, lg: Col2Label }
      , helpText: fragment
          [ DOOM.text "Whether to run some of the steps automatically."
          , DOOM.br {}
          , DOOM.text "In non-auto mode, we provide technical details about the requests and responses"
          , DOOM.br {}
          , DOOM.text "which deal with during the contract execution."
          ]
      , initial: true
      , touched: true
      }
  liftEffect $ component "ApplyInputs.ContractDetailsComponent" \{ marloweContext: { contract, state }, onSuccess, onDismiss } -> React.do
    { formState, onSubmit: onSubmit' } <- useStatelessFormSpec
      { spec: autoRunFormSpec
      , onSubmit: _.result >>> case _ of
          Just (V (Right autoRun) /\ _) -> onSuccess autoRun
          _ -> pure unit
      , validationDebounce: Seconds 0.5
      }

    let
      fields = StatelessFormSpec.renderFormSpec autoRunFormSpec formState
      body = fragment $
        [ contractSection contract state Nothing
        , DOOM.hr {}
        ]
          <> fields
      footer = fragment
        [ DOM.div { className: "row" } $
            [ DOM.div { className: "col-6 text-start" } $
                [ link
                    { label: DOOM.text "Cancel"
                    , onClick: onDismiss
                    , showBorders: true
                    , extraClassNames: "me-3"
                    }
                ]
            , DOM.div { className: "col-6 text-end" } $
                [ DOM.button
                    { className: "btn btn-primary"
                    , onClick: onSubmit'
                    , disabled: false
                    }
                    [ R.text "Submit" ]
                ]
            ]
        ]
    pure $ BodyLayout.component
      { title: DOM.div { className: "px-3 mx-3 fw-bold" }
          [ DOOM.img { src: "/images/magnifying_glass.svg" }
          , DOM.h3 { className: "fw-bold" } $ DOOM.text "Advance the contract"
          ]
      , description: DOM.div { className: "px-3 mx-3" }
          [ DOM.p {} [ DOOM.text "Progress through the contract by delving into its specifics. Analyse the code, evaluate the graph and apply the required inputs. This stage is crucial for ensuring the contract advances correctly so take a moment to confirm all details." ]
          ]
      , content: wrappedContentWithFooter body footer
      }

-- Now we want to to describe the interaction with the API where runtimeRequest is
-- a { headers: Map String String, body: JSON }.
-- We really want to provide the detailed informatin (headers and payoload)
creatingTxDetails :: forall a1531 a1573. Maybe (Effect Unit) -> Effect Unit -> a1531 -> Maybe a1573 -> JSX
creatingTxDetails possibleOnNext onDismiss runtimeRequest possibleRuntimeResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
          [ DOM.p { className: "h3" } $ DOOM.text "API request:"
          , DOM.p {} $ jsonSyntaxHighlighter $ unsafeStringify runtimeRequest
          ]
      , DOM.div { className: "col-6" } $ case possibleRuntimeResponse of
          Nothing -> -- FIXME: loader

            DOM.p {} $ DOOM.text "No response yet."
          Just runtimeResponse -> fragment
            [ DOM.p { className: "h3" } $ DOOM.text "API response:"
            , DOM.p {} $ jsonSyntaxHighlighter $ unsafeStringify runtimeResponse
            ]
      ]
    footer = fragment
      [ DOM.div { className: "row" } $
          [ DOM.div { className: "col-6 text-start" } $
              [ link
                  { label: DOOM.text "Cancel"
                  , onClick: onDismiss
                  , showBorders: true
                  , extraClassNames: "me-3"
                  }
              ]
          , DOM.div { className: "col-6 text-end" } $
              [ case possibleOnNext of
                  Nothing -> DOM.button
                    { className: "btn btn-primary"
                    , disabled: true
                    }
                    [ R.text "Dismiss" ]
                  Just onNext -> DOM.button
                    { className: "btn btn-primary"
                    , onClick: handler_ onNext
                    , disabled: false
                    }
                    [ R.text "Next" ]
              ]
          ]
      ]
  DOM.div { className: "row" } $ BodyLayout.component
    { title: DOM.h3 {} $ DOOM.text "Creating Transaction"
    , description: DOOM.div_
        [ DOM.p {} [ DOOM.text "We use the Marlowe Runtime to request a transaction that will apply the chosen input." ]
        , DOM.p {} [ DOOM.text "In order to build the required transaction we use Marlowe Runtime REST API. We encode the input which we wish to apply and also provide the addresses which we were able to collect in the previous step from the wallet. The addresses are re-encoded from the lower-level Cardano CBOR hex format into Bech32 format (", DOM.code {} [ DOOM.text "addr_test..." ], DOOM.text ") and sent to the backend as part of the request." ]
        , DOM.p {} [ DOOM.text "On the transction level this application of input is carried out by providing a redeemer, which encodes the chosen input and supplies it to the Marlowe script to execute the contract step(s). The transaction outputs must fulfill the requirements of the effects of this input application. Specifically, they need to handle all payouts if any are made, or deposit the required deposit, or finalize the contract and payout all the money according to the accounting state." ]
        ]
    , content: wrappedContentWithFooter body footer
    }

type Href = String

-- DOM.a { href: "https://preview.marlowescan.com/contractView?tab=info&contractId=09127ec2bd83d20dc108e67fe73f7e40280f6f48ea947606a7b73ac5268985a0%231", target: "_blank", className: "white-color" } [ DOOM.i { className: "ms-1 h6 bi-globe2" }, DOOM.text "  Marlowe Explorer" ]

signingTransaction :: forall res. Maybe (Effect Unit) -> Effect Unit -> Maybe res -> JSX
signingTransaction possibleOnNext onDismiss possibleWalletResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" } $ case possibleWalletResponse of
          Nothing ->
            DOM.div
              { className: "col-12 position-absolute top-0 start-0 w-100 h-100 d-flex justify-content-center align-items-center blur-bg z-index-sticky"
              }
              $ loadingSpinnerLogo
                  {}
          Just runtimeResponse -> fragment
            [ DOM.p { className: "h3" } $ DOOM.text "API response:"
            , DOM.p {} $ jsonSyntaxHighlighter $ unsafeStringify runtimeResponse
            ]
      ]
    footer = fragment
      [ link
          { label: DOOM.text "Cancel"
          , onClick: onDismiss
          , showBorders: true
          , extraClassNames: "me-3"
          }
      , case possibleOnNext of
          Nothing -> DOM.button
            { className: "btn btn-primary"
            , disabled: true
            }
            [ R.text "Dismiss" ]
          Just onNext -> DOM.button
            { className: "btn btn-primary"
            , onClick: handler_ onNext
            , disabled: false
            }
            [ R.text "Next" ]
      ]
  DOM.div { className: "row" } $ BodyLayout.component
    { title: DOM.h3 {} $ DOOM.text "Signing transaction"
    , description: fragment
        [ DOM.p {} [ DOOM.text "We are now signing the transaction with the wallet. While the wallet currently does not provide detailed information about the Marlowe contract within the transaction, all transaction details, including the contract, are readily accessible and can be decoded for verification:" ]
        , DOM.ul {}
            [ DOM.li {}
                [ DOOM.text "A consistent Marlowe validator is used across all transactions. As the UTxO with Marlowe is available on the chain, it can be cheaply referenced - please check "
                , descriptionLink { icon: "bi-github", href: "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031", label: "CIP-0031" }
                , DOOM.text " for more details."
                ]
            , DOM.li {}
                [ DOOM.text "The Marlowe contract, along with its state, is encoded in the datum of the UTxO with the validator."
                ]
            , DOM.li {}
                [ DOOM.text "The value on the UTxO should represent the amount of money that is locked in the contract."
                ]
            ]
        ]
    , content: wrappedContentWithFooter body footer
    }

submittingTransaction :: forall req res. Effect Unit -> req -> Maybe res -> JSX
submittingTransaction onDismiss runtimeRequest possibleRuntimeResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
          [ DOM.p { className: "h3" } $ DOOM.text "We are submitting the final transaction"
          , DOM.p {} $ jsonSyntaxHighlighter $ unsafeStringify runtimeRequest
          ]
      , DOM.div { className: "col-6" } $ case possibleRuntimeResponse of
          Nothing -> -- FIXME: loader

            DOM.p {} $ DOOM.text "No response yet."
          Just runtimeResponse -> fragment
            [ DOM.p { className: "h3" } $ DOOM.text "API response:"
            , DOM.p {} $ jsonSyntaxHighlighter $ unsafeStringify runtimeResponse
            ]
      ]
    footer = fragment
      [ link
          { label: DOOM.text "Cancel"
          , onClick: onDismiss
          , showBorders: true
          , extraClassNames: "me-3"
          }
      ]
  DOM.div { className: "row" } $ BodyLayout.component
    { title: fragment [ DOM.h3 {} $ DOOM.text "Submitting transaction signatures" ]
    , description: fragment
        [ DOM.p {} [ DOOM.text "We are submitting the signatures for the transaction to the Marlowe Runtime now using its REST API." ]
        , DOM.p {} [ DOOM.text "Marlowe Runtime will verify the signatures and if they are correct, it will attach them to the transaction and submit the transaction to the blockchain." ]
        ]
    , content: wrappedContentWithFooter body footer
    }

data PreviewMode
  = DetailedFlow { showPrevStep :: Boolean }
  | SimplifiedFlow

setShowPrevStep :: PreviewMode -> Boolean -> PreviewMode
setShowPrevStep (DetailedFlow _) showPrevStep = DetailedFlow { showPrevStep }
setShowPrevStep SimplifiedFlow _ = SimplifiedFlow

shouldShowPrevStep :: PreviewMode -> Boolean
shouldShowPrevStep (DetailedFlow { showPrevStep }) = showPrevStep
shouldShowPrevStep SimplifiedFlow = false

type Props =
  { onDismiss :: Effect Unit
  , onSuccess :: ContractInfo.ContractUpdated -> Effect Unit
  , onError :: String -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , transactionsEndpoint :: TransactionsEndpoint
  , marloweContext :: Machine.MarloweContext
  , contractInfo :: ContractInfo
  }

newtype UseSpinnerOverlay = UseSpinnerOverlay Boolean

applyInputBodyLayout :: UseSpinnerOverlay -> JSX -> JSX
applyInputBodyLayout (UseSpinnerOverlay useSpinnerOverlay) content = do
  let
    title = DOM.div { className: "" }
      [ DOM.div { className: "mb-3" } $ DOOM.img { src: "/images/magnifying_glass.svg" }
      , DOM.div { className: "mb-3" } $ DOOM.text "Advance the contract"
      ]
    description = DOM.p { className: "mb-3" } "Progress through the contract by delving into its specifics. Analyse the code, evaluate the graph and apply the required inputs. This stage is crucial for ensuring the contract advances correctly so take a moment to confirm all details."
    content' = fragment $
      [ content ]
        <> Monoid.guard useSpinnerOverlay [ spinnerOverlay Spinner100VH ]
  BodyLayout.component { title, description, content: content' }

mkOnStateTransition
  :: ContractInfo
  -> (ContractInfo.ContractUpdated -> Effect Unit)
  -> (String -> Effect Unit)
  -> Moore.OnStateTransition Machine.State
mkOnStateTransition contractInfo onSuccess _ _ (Machine.InputApplied ia) = do
  let
    { submittedAt
    , input: possibleInput
    , environment
    , newMarloweContext: { state, contract }
    } = ia
    V1.Environment { timeInterval } = environment
    transactionInput = V1.TransactionInput
      { inputs: foldr List.Cons List.Nil possibleInput
      , interval: timeInterval
      }
    contractUpdated = ContractInfo.ContractUpdated
      { contractInfo
      , transactionInput
      , outputContract: contract
      , outputState: state
      , submittedAt
      }
  onSuccess contractUpdated
mkOnStateTransition _ _ onErrors _ next = do
  void $ for (Machine.stateErrors next) onErrors

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  -- contractDetailsComponent <- mkContractDetailsComponent
  depositFormComponent <- mkDepositFormComponent
  choiceFormComponent <- mkChoiceFormComponent
  notifyFormComponent <- mkNotifyFormComponent
  advanceFormComponent <- mkAdvanceFormComponent

  liftEffect $ component "ApplyInputs" \{ connectedWallet, onSuccess, onError, onDismiss, marloweContext, contractInfo, transactionsEndpoint } -> React.do
    walletRef <- React.useRef connectedWallet
    let
      WalletInfo { name: walletName } = connectedWallet
    React.useEffect walletName do
      React.writeRef walletRef connectedWallet
      pure $ pure unit

    -- We starting by fetching wallet context so in submit mode
    submitting /\ setSubmitting <- useState' true

    machine <- do
      let
        onStateTransition prev next = do
          let
            fn = mkOnStateTransition contractInfo onSuccess onError
          case prev of
            Machine.FetchingRequiredWalletContext _ -> setSubmitting false
            _ -> pure unit
          fn prev next

        props = mkMachineSpec marloweContext transactionsEndpoint connectedWallet cardanoMultiplatformLib onStateTransition runtime
      useMooreMachine props

    let
      shouldUseSpinner = UseSpinnerOverlay submitting

    pure $ case machine.state of
      Machine.FetchingRequiredWalletContext {} -> do
        applyInputBodyLayout shouldUseSpinner $ mempty

      Machine.ChoosingInputType { allInputsChoices } -> do
        let
          body = fragment $
            [ contractSection marloweContext.contract marloweContext.state Nothing
            ]

          footer = DOM.div { className: "row" }
            [ DOM.div { className: "col-6 text-start" } $
                [ link
                    { label: DOOM.text "Cancel"
                    , onClick: onDismiss
                    , showBorders: true
                    , extraClassNames: "me-3"
                    }
                ]
            , DOM.div { className: "col-6 text-end" } $ do
                [ DOM.button
                    { className: "btn btn-primary me-2"
                    , disabled: not $ Machine.canDeposit allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Right { deposits: Just deposits } ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.DepositInputs deposits)
                        _ -> pure unit
                    }
                    [ R.text "Deposit" ]
                , DOM.button
                    { className: "btn btn-primary me-2"
                    , disabled: not $ Machine.canChoose allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Right { choices: Just choices } ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.ChoiceInputs choices)
                        _ -> pure unit
                    }
                    [ R.text "Choice" ]
                , DOM.button
                    { className: "btn btn-primary me-2"
                    , disabled: not $ Machine.canNotify allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Right { notify: Just notify } ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.SpecificNotifyInput notify)
                        _ -> pure unit
                    }
                    [ R.text "Notify" ]
                , DOM.button
                    { className: "btn btn-primary me-2"
                    , disabled: not $ Machine.canAdvance allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Left advanceContinuation ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.AdvanceContract advanceContinuation)
                        _ -> pure unit
                    }
                    [ R.text "Advance" ]
                ]
            ]
        BodyLayout.component
          { title: DOM.h3 {} $ DOOM.text "Select Input Type"
          , description:
              DOM.div {}
                [ DOM.p {}
                    [ DOOM.text "You have reached a point in the contract where an input is required to proceed. The contract may allow for various types of inputs depending on its current state and the logic it contains. Below, you will find a selection of input types that you can choose from to interact with the contract. Note that not all input types may be available at this point in the contract. The available input types are enabled, while the others are disabled." ]
                , DOM.ul {}
                    [ DOM.li {} [ DOM.strong {} [ DOOM.text "Deposit:" ], DOOM.text " If enabled, this option allows you to make a deposit into the contract. This might be required for certain conditions or actions within the contract." ]
                    , DOM.li {} [ DOM.strong {} [ DOOM.text "Choice:" ], DOOM.text " If enabled, this option allows you to make a choice from a set of predefined options. This choice can affect the flow of the contract." ]
                    , DOM.li {} [ DOM.strong {} [ DOOM.text "Notify:" ], DOOM.text " If enabled, this option allows you to notify the contract of a certain event or condition. This can be used to trigger specific actions within the contract." ]
                    , DOM.li {} [ DOM.strong {} [ DOOM.text "Advance:" ], DOOM.text " If enabled, this option allows you to move the contract forward to the next state without making any other input." ]
                    ]
                , DOM.p {}
                    [ DOOM.text "Please select the appropriate input type based on the current state of the contract and the action you wish to take. After selecting an input type, you may be required to provide additional information or make a choice before the contract can proceed." ]
                ]
          , content: wrappedContentWithFooter body footer
          }

      _ -> do
        let
          ctx = do
            environment <- Machine.stateEnvironment machine.state
            inputChoices <- Machine.stateInputChoices machine.state
            pure { environment, inputChoices }
        case ctx of
          Nothing -> DOOM.text "Should rather not happen ;-)"
          Just { environment, inputChoices } -> do
            let
              V1.Environment { timeInterval } = environment
              applyPickInputSucceeded input = do
                let
                  transactionInput = V1.TransactionInput
                    { inputs: foldr List.Cons List.Nil input
                    , interval: timeInterval
                    }
                  { initialContract, state, contract } = marloweContext
                case V1.computeTransaction transactionInput state contract of
                  V1.TransactionOutput t -> do
                    let
                      newMarloweContext = { initialContract, state: t.txOutState, contract: t.txOutContract }
                    machine.applyAction <<< Machine.PickInputSucceeded $ { input, newMarloweContext }
                  V1.Error err -> do
                    machine.applyAction <<< Machine.PickInputFailed $ show err
            case inputChoices of
              ChoiceInputs choiceInputs -> applyInputBodyLayout shouldUseSpinner $ choiceFormComponent
                { choiceInputs
                , connectedWallet
                , marloweContext
                , onDismiss
                , onSubmit: \input -> do
                    setSubmitting true
                    applyPickInputSucceeded <<< Just $ input
                }
              DepositInputs depositInputs -> applyInputBodyLayout shouldUseSpinner $ depositFormComponent
                { depositInputs
                , connectedWallet
                , marloweContext
                , onDismiss
                , onSubmit: \input -> do
                    setSubmitting true
                    applyPickInputSucceeded <<< Just $ input
                }
              SpecificNotifyInput notifyInput -> applyInputBodyLayout shouldUseSpinner $ notifyFormComponent
                { notifyInput
                , connectedWallet
                , marloweContext
                , onDismiss
                , onSubmit: do
                    setSubmitting true
                    applyPickInputSucceeded <<< Just $ V1.NormalInput V1.INotify
                , timeInterval
                }
              AdvanceContract _ -> applyInputBodyLayout shouldUseSpinner $ advanceFormComponent
                { marloweContext
                , timeInterval
                , onDismiss
                , onSubmit: do
                    setSubmitting true
                    applyPickInputSucceeded Nothing
                }

address :: String
address = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

mkInitialContract :: Effect V1.Contract
mkInitialContract = do
  nowMilliseconds <- unInstant <$> now
  let
    timeout = case instant (nowMilliseconds <> Milliseconds (Int.toNumber $ 5 * 60 * 1000)) of
      Just i -> i
      Nothing -> unsafeCrashWith "Invalid instant"

  pure $ V1.When
    [ V1.Case
        ( V1.Deposit
            (V1.Address address)
            (V1.Address address)
            (V1.Token "" "")
            (V1.Constant $ BigInt.fromInt 1000000)
        )
        V1.Close
    ]
    timeout
    V1.Close

newtype ApplyInputsContext = ApplyInputsContext
  { wallet :: { changeAddress :: Bech32, usedAddresses :: Array Bech32 }
  , inputs :: Array V1.Input
  , timeInterval :: V1.TimeInterval
  }

applyInputs
  :: ApplyInputsContext
  -> ServerURL
  -> TransactionsEndpoint
  -> Aff
       ( Either ClientError'
           { links ::
               { transaction :: TransactionEndpoint
               }
           , resource :: PostTransactionsResponse
           }
       )

applyInputs (ApplyInputsContext ctx) serverURL transactionsEndpoint = do
  let
    req = PostTransactionsRequest
      { inputs: ctx.inputs
      , invalidBefore: Nothing
      , invalidHereafter: Nothing
      , metadata: mempty
      , tags: mempty
      , changeAddress: ctx.wallet.changeAddress
      , addresses: ctx.wallet.usedAddresses
      , collateralUTxOs: []
      }
  post' serverURL transactionsEndpoint req

