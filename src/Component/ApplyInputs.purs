module Component.ApplyInputs where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs.Machine (AutoRun(..), InputChoices(..))
import Component.ApplyInputs.Machine as Machine
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.InputHelper (ChoiceInput(..), DepositInput(..), NotifyInput, toIChoice, toIDeposit)
import Component.MarloweYaml (marloweStateYaml, marloweYaml)
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder')
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec)
import Contrib.React.Basic.Hooks.UseMooreMachine (useMooreMachine)
import Contrib.React.MarloweGraph (marloweGraph)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), FieldLayout(..), LabelSpacing(..), booleanField, choiceField, intInput, radioFieldChoice, selectFieldChoice)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable as Argonaut
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types (Action(..), Ada(..), Case(..), ChoiceId(..), Contract(..), Input(..), InputContent(..), Party(..), TimeInterval(..), Token(..), Value(..)) as V1
import Language.Marlowe.Core.V1.Semantics.Types (Input(..))
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent, PostTransactionsRequest(PostTransactionsRequest), PostTransactionsResponse, PutTransactionRequest(PutTransactionRequest), Runtime(Runtime), ServerURL, TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnMMaybe, liftFnMaybe)
import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useContext, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Result = V1.Contract

data ContractData = ContractData
  { contract :: V1.Contract
  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  -- , collateralUTxOs :: Array TxOutRef
  }

-- TODO: Introduce proper error type to the Marlowe.Runtime.Web.Types for this post response
type ClientError' = ClientError String

create :: ContractData -> ServerURL -> ContractsEndpoint -> Aff (Either ClientError' { resource :: PostContractsResponseContent, links :: { contract :: ContractEndpoint } })
create contractData serverUrl contractsEndpoint = do
  let
    ContractData { contract, changeAddress, usedAddresses } = contractData
    req = PostContractsRequest
      { metadata: mempty
      -- , version :: MarloweVersion
      , roles: Nothing
      , tags: mempty -- TODO: use instead of metadata
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

type DepositFormComponentProps =
  { depositInputs :: NonEmptyArray DepositInput
  , connectedWallet :: WalletInfo Wallet.Api
  , onDismiss :: Effect Unit
  , onSuccess :: V1.Input -> Effect Unit
  }

mkDepositFormComponent :: MkComponentM (DepositFormComponentProps -> JSX)
mkDepositFormComponent = do
  liftEffect $ component "ApplyInputs.DepositFormComponent" \{ depositInputs, onDismiss, onSuccess } -> React.do
    let
      choices = RadioButtonFieldChoices do
        let
          toChoice idx (DepositInput _ _ _ value _) = do
            let
              label = show value
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
        choiceField { choices, validator }

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right deposit) /\ _) -> case toIDeposit deposit of
          Just ideposit -> onSuccess $ NormalInput ideposit
          Nothing -> pure unit
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }
    pure $ BodyLayout.component do
      let
        fields = renderFormSpec formSpec formState
        body = DOM.div { className: "form-group" } fields
        actions = fragment
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
      { title: "Perform deposit"
      , description: DOOM.text "We are creating the initial transaction."
      , content: wrappedContentWithFooter body actions
      }

type ChoiceFormComponentProps =
  { choiceInputs :: NonEmptyArray ChoiceInput
  , connectedWallet :: WalletInfo Wallet.Api
  , onDismiss :: Effect Unit
  , onSuccess :: V1.Input -> Effect Unit
  }

mkChoiceFormComponent :: MkComponentM (ChoiceFormComponentProps -> JSX)
mkChoiceFormComponent = do
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.DepositFormComponent" \{ choiceInputs, connectedWallet, onDismiss, onSuccess } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    -- type ChoiceFieldProps validatorM a =
    --   { choices :: ChoiceFieldChoices
    --   , validator :: Batteries.Validator validatorM String (Maybe String) a
    --   | ChoiceFieldOptionalPropsRow ()
    --   }
    partialFormResult /\ setPartialFormResult <- useState' Nothing
    let
      choices = SelectFieldChoices do
        let
          toChoice idx (ChoiceInput (V1.ChoiceId name _) _ _) = do
            selectFieldChoice name (show idx)
        ArrayAL.fromNonEmptyArray $ mapWithIndex toChoice choiceInputs

      validator :: Batteries.Validator Effect _ _ _
      validator = do
        let
          value2Deposit = Map.fromFoldable $ mapWithIndexFlipped choiceInputs \idx choiceInput -> show idx /\ choiceInput
        liftFnMMaybe (\v -> pure [ "Invalid choice: " <> show v ]) \possibleIdx -> runMaybeT do
          deposit <- MaybeT $ pure do
            idx <- possibleIdx
            Map.lookup idx value2Deposit
          liftEffect $ setPartialFormResult $ Just deposit
          pure deposit

      formSpec = evalBuilder' $ ado
        choice <- choiceField { choices, validator, touched: true, initial: "0" }
        value <- intInput {}
        in
          { choice, value }

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right { choice, value }) /\ _) -> case toIChoice choice (BigInt.fromInt value) of
          Just ichoice -> onSuccess $ NormalInput ichoice
          Nothing -> pure unit
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }
    pure $ BodyLayout.component do
      let
        fields = renderFormSpec formSpec formState
        body = DOOM.div_ $
          [ DOM.div { className: "form-group" } fields ]
            <> case partialFormResult of
              Just (ChoiceInput _ _ (Just cont)) ->
                [ DOOM.hr {}
                , DOOM.text $ show cont
                ]
              _ -> mempty

        actions = fragment
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
      { title: "Perform choice"
      , description: DOOM.text "Perform choice description"
      , content: wrappedContentWithFooter body actions
      }

type NotifyFormComponentProps =
  { notifyInput :: NotifyInput
  , connectedWallet :: WalletInfo Wallet.Api
  , onDismiss :: Effect Unit
  , onSuccess :: Effect Unit
  }

mkNotifyFormComponent :: MkComponentM (NotifyFormComponentProps -> JSX)
mkNotifyFormComponent = do
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.NotifyFormComponent" \{ notifyInput, connectedWallet, onDismiss, onSuccess } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    pure $ BodyLayout.component do
      let
        body = DOOM.text ""
        actions = fragment
          [ DOM.button
              { className: "btn btn-primary"
              , onClick: handler_ onSuccess
              , disabled: false
              }
              [ R.text "Submit" ]
          ]
      { title: "Perform notify"
      , description: DOOM.text "Perform notify description"
      , content: wrappedContentWithFooter body actions
      }

type AdvanceFormComponentProps =
  { contract :: V1.Contract
  , onDismiss :: Effect Unit
  , onSuccess :: Effect Unit
  }

mkAdvanceFormComponent :: MkComponentM (AdvanceFormComponentProps -> JSX)
mkAdvanceFormComponent = do
  liftEffect $ component "ApplyInputs.AdvanceFormComponent" \{ contract, onDismiss, onSuccess } -> React.do
    pure $ BodyLayout.component do
      let
        body = DOOM.text ""
        actions = fragment
          [ DOM.button
              { className: "btn btn-primary"
              , onClick: handler_ onSuccess
              , disabled: false
              }
              [ R.text "Submit" ]
          ]
      { title: "Advance contract"
      , description: DOOM.text "Advance Contract description"
      , content: wrappedContentWithFooter body actions
      }

data CreateInputStep
  = SelectingInputType
  | PerformingDeposit (NonEmptyArray DepositInput)
  | PerformingNotify (NonEmptyArray NotifyInput)
  | PerformingChoice (NonEmptyArray ChoiceInput)
  | PerformingAdvance V1.Contract

data Step = Creating CreateInputStep

-- | Created (Either String PostContractsResponseContent)
-- | Signing (Either String PostContractsResponseContent)
-- | Signed (Either ClientError PostContractsResponseContent)

type Props =
  { onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , transactionsEndpoint :: TransactionsEndpoint
  , marloweContext :: Machine.MarloweContext
  }

machineProps marloweContext transactionsEndpoint connectedWallet cardanoMultiplatformLib runtime = do
  let
    env = { connectedWallet, cardanoMultiplatformLib, runtime }
    -- allInputsChoices = case nextTimeoutAdvance environment contract of
    --   Just advanceContinuation -> Left advanceContinuation
    --   Nothing -> do
    --     let
    --       deposits = NonEmpty.fromArray $ nextDeposit environment state contract
    --       choices = NonEmpty.fromArray $ nextChoice environment state contract
    --       notify = NonEmpty.head <$> NonEmpty.fromArray (nextNotify environment state contract)
    --     Right { deposits, choices, notify }

  { initialState: Machine.initialState marloweContext transactionsEndpoint
  , step: Machine.step
  , driver: Machine.driver env
  , output: identity
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
  liftEffect $ component "ApplyInputs.ContractDetailsComponent" \{ marloweContext: { initialContract, contract, state }, onSuccess, onDismiss } -> React.do
    { formState, onSubmit: onSubmit' } <- useStatelessFormSpec
      { spec: autoRunFormSpec
      , onSubmit: _.result >>> case _ of
          Just (V (Right autoRun) /\ _) -> onSuccess autoRun
          _ -> pure unit
      , validationDebounce: Seconds 0.5
      }

    let
      contractSection =
        tabs { fill: true, justify: true, defaultActiveKey: "graph", variant: Tabs.variant.pills } do
          let
            renderTab props children = tab props $ DOM.div { className: "pt-4 w-100 h-vh50 overflow-auto" } children
          [ renderTab
              { eventKey: eventKey "graph"
              , title: DOOM.span_
                  [ Icons.toJSX $ unsafeIcon "diagram-2"
                  , DOOM.text " Source graph"
                  ]
              }
            [ marloweGraph { contract: contract } ]
          , renderTab
              { eventKey: eventKey "source"
              , title: DOOM.span_
                  [ Icons.toJSX $ unsafeIcon "filetype-yml"
                  , DOOM.text " Source code"
                  ]
              }
              [ marloweYaml contract ]
          , renderTab
              { eventKey: eventKey "state"
              , title: DOOM.span_
                  [ Icons.toJSX $ unsafeIcon "bank"
                  , DOOM.text " Contract state"
                  ]
              }
              [ marloweStateYaml state ]
          ]
      fields = renderFormSpec autoRunFormSpec formState
      body = fragment $
        [ contractSection
        , DOOM.hr {}
        ]
        <>  fields
      footer = fragment
        [ link
            { label: DOOM.text "Cancel"
            , onClick: onDismiss
            , showBorders: true
            , extraClassNames: "me-3"
            }
        , DOM.button
            { className: "btn btn-primary"
            , onClick: onSubmit'
            , disabled: false
            }
            [ R.text "Submit" ]
        ]
    pure $ BodyLayout.component
      { title: "Apply Input"
      , description: DOM.div {}
          [ DOM.p {} [ DOOM.text "This page allows contract participants to interact with the contract and take actions to progress through it. On the right side of the page, you will see a representation of the contract state as it currently exists on the blockchain. The page is divided into three tabs: Source Graph, Source Code, and Contract State. Each tab provides a different view of the contract." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Source Graph" ]
          , DOM.p {} [ DOOM.text "The Source Graph tab provides a visual representation of the contract. It displays the contract as a graph, with nodes representing the different states and actions of the contract. The paths that have already been executed (transactions) are highlighted, allowing you to see the progression of the contract over time." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Source Code" ]
          , DOM.p {} [ DOOM.text "In the Source Code tab, you can view the remaining part of the contract that is on the blockchain. This includes the logic and conditions that are yet to be executed. It's a textual representation of the contract, allowing you to understand the contract's structure and logic." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Contract State" ]
          , DOM.p {} [ DOOM.text "In the Contract State tab, you can view the current status of the participant's account, as well as the chosen values and variables that have been set within the contract (using 'let' statements). This tab provides a snapshot of the contract's current state and the participant's interaction with it." ]
          ]
      , content: wrappedContentWithFooter body footer
      }

-- In here we want to summarize the initial interaction with the wallet
fetchingRequiredWalletContextDetails possibleOnNext onDismiss possibleWalletResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
        -- | Let's describe that we are
          [ DOM.p {} $ DOOM.text "We are fetching the required wallet context."
          , DOM.p {} $ DOOM.text "marlowe-runtime requires information about wallet addresses so it can pick UTxO to pay for the initial transaction."
          , DOM.p {} $ DOOM.text $
              "To gain the address set from the wallet we use CIP-30 `getUsedAddresses` method and reencoding them from lower "
                <> "level cardano CBOR hex into Bech32 (`addr_test...`)."
          ]
      , DOM.div { className: "col-6" } $ case possibleWalletResponse of
          Nothing ->
            -- FIXME: loader
            DOM.p {} $ DOOM.text "No response yet."
          Just walletResponse -> fragment
            [ DOM.p {} $ DOOM.text "Wallet response:"
            , DOM.p {} $ DOOM.text $ unsafeStringify walletResponse
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
            [ R.text "Next" ]
          Just onNext -> DOM.button
            { className: "btn btn-primary"
            , onClick: handler_ onNext
            , disabled: false
            }
            [ R.text "Next" ]
      ]

  BodyLayout.component
    { title: "Apply Inputs | Fetching Wallet Context"
    , description: DOOM.text "Fetching Wallet Context description"
    , content: wrappedContentWithFooter body footer
    }

-- Now we want to to describe the interaction with the API where runtimeRequest is
-- a { headers: Map String String, body: JSON }.
-- We really want to provide the detailed informatin (headers and payoload)
creatingTxDetails possibleOnNext onDismiss runtimeRequest possibleRuntimeResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
        [ DOM.p {} $ DOOM.text "We are creating the initial transaction."
        , DOM.p {} $ DOOM.text "We are sending the following request headers to the API:"
        , DOM.p {} $ DOOM.text $ unsafeStringify runtimeRequest
        ]
      , DOM.div { className: "col-6" } $ case possibleRuntimeResponse of
          Nothing -> -- FIXME: loader
            DOM.p {} $ DOOM.text "No response yet."
          Just runtimeResponse -> fragment
            [ DOM.p {} $ DOOM.text "API response:"
            , DOM.p {} $ DOOM.text $ unsafeStringify runtimeResponse
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
    { title: "Apply Inputs | Creating Transaction"
    , description: DOOM.text "We are creating the initial transaction."
    , content: wrappedContentWithFooter body footer
    }

signingTransaction possibleOnNext onDismiss possibleWalletResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" } $ case possibleWalletResponse of
          Nothing -> -- FIXME: loader
            DOM.p {} $ DOOM.text "No response yet."
          Just runtimeResponse -> fragment
            [ DOM.p {} $ DOOM.text "API response:"
            , DOM.p {} $ DOOM.text $ unsafeStringify runtimeResponse
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
    { title: "Apply Inputs | Creating Transaction"
    , description: DOOM.text "We are singning the transaction."
    , content: wrappedContentWithFooter body footer
    }

submittingTransaction onDismiss runtimeRequest possibleRuntimeResponse = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
        [ DOM.p {} $ DOOM.text "We submitting the final transaction"
        , DOM.p {} $ DOOM.text $ unsafeStringify runtimeRequest
        ]
      , DOM.div { className: "col-6" } $ case possibleRuntimeResponse of
          Nothing -> -- FIXME: loader
            DOM.p {} $ DOOM.text "No response yet."
          Just runtimeResponse -> fragment
            [ DOM.p {} $ DOOM.text "API response:"
            , DOM.p {} $ DOOM.text $ unsafeStringify runtimeResponse
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
    { title: "Apply Inputs | Submitting Transaction"
    , description: DOOM.text "We are submitting the final signed transaction."
    , content: wrappedContentWithFooter body footer
    }

inputApplied onNext = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
        [ DOM.p {} $ DOOM.text "Input applied successfully?"
        ]
      , DOM.div { className: "col-6" } $ fragment
            [ DOM.p {} $ DOOM.text "API response:"
            , DOM.p {} $ DOOM.text $ unsafeStringify "201"
            ]
      ]
    footer = fragment
      [ DOM.button
          { className: "btn btn-primary"
          , onClick: handler_ onNext
          , disabled: false
          }
          [ R.text "Ok" ]
      ]
  DOM.div { className: "row" } $ BodyLayout.component
    { title: "Apply Inputs | Submitting Transaction"
    , description: DOOM.text "We are submitting the final signed transaction."
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

showPossibleErrorAndDismiss title body onDismiss errors = do
  let
    body' = case errors of
      Just errors -> fragment
        [ DOM.p {} $ DOOM.text "Error:"
        , DOM.p {} $ DOOM.text $ unsafeStringify errors
        ]
      Nothing -> body
    footer = case errors of
      Just errors -> fragment
        [ link
            { label: DOOM.text "Cancel"
            , onClick: onDismiss
            , showBorders: true
            , extraClassNames: "me-3"
            }
        ]
      Nothing -> mempty
  DOM.div { className: "row" } $ BodyLayout.component
    { title: "Apply Inputs | Submitting Transaction"
    , description: DOOM.text "We are submitting the final signed transaction."
    , content: wrappedContentWithFooter body' footer
    }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  contractDetailsComponent <- mkContractDetailsComponent
  depositFormComponent <- mkDepositFormComponent
  choiceFormComponent <- mkChoiceFormComponent
  notifyFormComponent <- mkNotifyFormComponent
  advanceFormComponent <- mkAdvanceFormComponent

  liftEffect $ component "ApplyInputs" \{ connectedWallet, onSuccess, onDismiss, marloweContext, transactionsEndpoint } -> React.do
    walletRef <- React.useRef connectedWallet
    previewFlow /\ setPreviewFlow <- React.useState' $ DetailedFlow { showPrevStep: true }
    let
      WalletInfo { name: walletName } = connectedWallet
    React.useEffect walletName do
      React.writeRef walletRef connectedWallet
      pure $ pure unit

    machine <- do
      let
        props = machineProps marloweContext transactionsEndpoint connectedWallet cardanoMultiplatformLib runtime
      useMooreMachine props

    let
      setNextFlow = case previewFlow of
        DetailedFlow { showPrevStep: true } -> do
          traceM "NEXT STEP"
          traceM previewFlow
          setPreviewFlow $ DetailedFlow { showPrevStep: false }
        DetailedFlow { showPrevStep: false } -> do
          traceM "NEXT STEP"
          traceM previewFlow
          setPreviewFlow $ DetailedFlow { showPrevStep: true }
        SimplifiedFlow -> pure unit

    pure $ case machine.state of
      Machine.PresentingContractDetails { } -> do
        let
          onStepSuccess (AutoRun autoRun) = do
            machine.applyAction $ Machine.FetchRequiredWalletContext { autoRun: AutoRun true, marloweContext, transactionsEndpoint }
            traceM "AUTO RUN:"
            traceM autoRun
            if autoRun
              then setPreviewFlow SimplifiedFlow
              else setPreviewFlow $ DetailedFlow { showPrevStep: true }

        contractDetailsComponent { marloweContext, onSuccess: onStepSuccess, onDismiss }
      Machine.FetchingRequiredWalletContext { errors } -> case previewFlow of
        DetailedFlow _ -> fetchingRequiredWalletContextDetails Nothing onDismiss Nothing
        SimplifiedFlow -> do
          let
            body = DOOM.text "Auto fetching... (progress bar?)"
          showPossibleErrorAndDismiss "Fetching wallet context" body onDismiss errors

      Machine.ChoosingInputType {allInputsChoices, requiredWalletContext} -> case previewFlow of
        DetailedFlow { showPrevStep: true } -> do
          fetchingRequiredWalletContextDetails (Just setNextFlow) onDismiss $ Just requiredWalletContext
        _ -> do
          let
            body = DOM.div { className: "row" }
              [ DOM.div { className: "col-12" } $ marloweYaml marloweContext.contract ]

            footer = DOM.div { className: "row" }
              [ DOM.div { className: "col-3 text-center" } $
                  DOM.button
                    { className: "btn btn-primary"
                    , disabled: not $ Machine.canDeposit allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Right { deposits: Just deposits } ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.DepositInputs deposits)
                        _ -> pure unit
                    }
                    [ R.text "Deposit" ]
              , DOM.div { className: "col-3 text-center" } $
                  DOM.button
                    { className: "btn btn-primary"
                    , disabled: not $ Machine.canChoose allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Right { choices: Just choices } ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.ChoiceInputs choices)
                        _ -> pure unit
                    }
                    [ R.text "Choice" ]
              , DOM.div { className: "col-3 text-center" } $
                  DOM.button
                    { className: "btn btn-primary"
                    , disabled: not $ Machine.canNotify allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Right { notify: Just notify } ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.SpecificNotifyInput notify)
                        _ -> pure unit
                    }
                    [ R.text "Notify" ]
              , DOM.div { className: "col-3 text-center" } $
                  DOM.button
                    { className: "btn btn-primary"
                    , disabled: not $ Machine.canAdvance allInputsChoices
                    , onClick: handler_ $ case allInputsChoices of
                        Left advanceContinuation ->
                          machine.applyAction (Machine.ChooseInputTypeSucceeded $ Machine.AdvanceContract advanceContinuation)
                        _ -> pure unit
                    }
                    [ R.text "Advance" ]
              ]

          BodyLayout.component
            { title: "Select input type"
            , description: DOOM.text "We are creating the initial transaction."
            , content: wrappedContentWithFooter body footer
            }
      Machine.PickingInput { inputChoices } -> do
        let
          applyPickInputSucceeded input = do
            setNextFlow
            machine.applyAction <<< Machine.PickInputSucceeded $ input
        case inputChoices of
          DepositInputs depositInputs ->
            depositFormComponent { depositInputs, connectedWallet, onDismiss, onSuccess: applyPickInputSucceeded <<< Just}
          ChoiceInputs choiceInputs ->
            choiceFormComponent { choiceInputs, connectedWallet, onDismiss, onSuccess: applyPickInputSucceeded <<< Just}
          SpecificNotifyInput notifyInput ->
            notifyFormComponent { notifyInput, connectedWallet, onDismiss, onSuccess: applyPickInputSucceeded <<< Just $ V1.NormalInput V1.INotify }
          AdvanceContract cont ->
            advanceFormComponent { contract: cont, onDismiss, onSuccess: applyPickInputSucceeded Nothing }
      Machine.CreatingTx { allInputsChoices, errors } -> case previewFlow of
        DetailedFlow _ -> do
          creatingTxDetails Nothing onDismiss "createTx placeholder" $ case errors of
            Just err -> Just $ err
            Nothing -> Nothing
        SimplifiedFlow -> do
          let
            body = DOOM.text "Auto creating tx..."
          showPossibleErrorAndDismiss "Creating Transaction" body onDismiss errors
        -- SimplifiedFlow -> BodyLayout.component
        --   { title: "Creating transaction"
        --   , description: DOOM.text "We are creating the initial transaction."
        --   , content: DOOM.text "Auto creating tx... (progress bar?)"
        --   }
      Machine.SigningTx { createTxResponse, errors } -> case previewFlow of
        DetailedFlow { showPrevStep: true } -> do
          creatingTxDetails (Just setNextFlow) onDismiss "createTx placeholder" $ Just createTxResponse
        DetailedFlow _ ->
          signingTransaction Nothing onDismiss Nothing
        SimplifiedFlow -> do
          let
            body = DOOM.text "Auto signing tx... (progress bar?)"
          showPossibleErrorAndDismiss "Signing Transaction" body onDismiss errors
      Machine.SubmittingTx { txWitnessSet, errors } -> case previewFlow of
        DetailedFlow { showPrevStep: true } -> do
          signingTransaction (Just setNextFlow) onDismiss $ Just txWitnessSet
        DetailedFlow _ ->
          submittingTransaction onDismiss "Final request placeholder" $ errors
        SimplifiedFlow -> BodyLayout.component
          { title: "Submitting transaction"
          , description: DOOM.text "We are submitting the initial transaction."
          , content: DOOM.text "Auto submitting tx... (progress bar?)"
          }
      Machine.InputApplied { } -> case previewFlow of
        DetailedFlow { showPrevStep: true } -> do
          submittingTransaction onDismiss "Final request placeholder" (Just "201")
        _ ->
          inputApplied onDismiss


    --     let
    --       body = DOM.div { className: "row" }
    --         [ DOM.div { className: "col-12" } $ yamlSyntaxHighlighter contract { sortKeys: mkFn2 sortMarloweKeys }
    --         ]

    --       footer = DOM.div { className: "row" }
    --         [ DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleDeposits || isJust possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleDeposits of
    --                   Just deposits -> setStep (Creating $ PerformingDeposit deposits)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Deposit" ]
    --         , DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleChoiceInputs || isJust possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleChoiceInputs of
    --                   Just choiceInputs -> setStep (Creating $ PerformingChoice choiceInputs)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Choice" ]
    --         , DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleNotifyInputs || isJust possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleNotifyInputs of
    --                   Just notifyInputs -> setStep (Creating $ PerformingNotify notifyInputs)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Notify" ]
    --         , DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleNextTimeoutAdvance of
    --                   Just cont -> setStep (Creating $ PerformingAdvance cont)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Advance" ]
    --         ]

    --     if inModal then modal
    --       { title: R.text "Select input type"
    --       , onDismiss
    --       , body
    --       , footer
    --       , size: Modal.ExtraLarge
    --       }
    --     else
    --       body


    -- step /\ setStep <- useState' (Creating SelectingInputType)
    -- let

    --   possibleDeposits = do
    --     let
    --       dps = nextDeposit environment state contract
    --     NonEmpty.fromArray $ dps

    --   possibleChoiceInputs = do
    --     let
    --       cis = nextChoice environment state contract
    --     NonEmpty.fromArray $ cis

    --   possibleNotifyInputs = do
    --     let
    --       cis = nextNotify environment state contract
    --     NonEmpty.fromArray $ cis

    --   possibleNextTimeoutAdvance = nextTimeoutAdvance environment contract

    -- pure $ case step of
    --   Creating SelectingInputType -> do
    --     let
    --       body = DOM.div { className: "row" }
    --         [ DOM.div { className: "col-12" } $ yamlSyntaxHighlighter contract { sortKeys: mkFn2 sortMarloweKeys }
    --         ]

    --       footer = DOM.div { className: "row" }
    --         [ DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleDeposits || isJust possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleDeposits of
    --                   Just deposits -> setStep (Creating $ PerformingDeposit deposits)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Deposit" ]
    --         , DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleChoiceInputs || isJust possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleChoiceInputs of
    --                   Just choiceInputs -> setStep (Creating $ PerformingChoice choiceInputs)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Choice" ]
    --         , DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleNotifyInputs || isJust possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleNotifyInputs of
    --                   Just notifyInputs -> setStep (Creating $ PerformingNotify notifyInputs)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Notify" ]
    --         , DOM.div { className: "col-3 text-center" } $
    --             DOM.button
    --               { className: "btn btn-primary"
    --               , disabled: isNothing possibleNextTimeoutAdvance
    --               , onClick: handler_ $ case possibleNextTimeoutAdvance of
    --                   Just cont -> setStep (Creating $ PerformingAdvance cont)
    --                   Nothing -> pure unit
    --               }
    --               [ R.text "Advance" ]
    --         ]

    --     if inModal then modal
    --       { title: R.text "Select input type"
    --       , onDismiss
    --       , body
    --       , footer
    --       , size: Modal.ExtraLarge
    --       }
    --     else
    --       body
    --   Creating (PerformingDeposit deposits) -> do
    --     depositFormComponent { deposits, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
    --   Creating (PerformingNotify notifyInputs) -> do
    --     notifyFormComponent { notifyInputs, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
    --   Creating (PerformingChoice choiceInputs) -> do
    --     choiceFormComponent { choiceInputs, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
    --   Creating (PerformingAdvance cont) -> do
    --     advanceFormComponent { contract: cont, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
    --   _ -> DOM.div { className: "row" } [ R.text "TEST" ]

address :: String
address = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

-- data TimeInterval = TimeInterval Instant Instant
defaultTimeInterval :: Effect V1.TimeInterval
defaultTimeInterval = do
  nowInstant <- now
  let
    nowMilliseconds = unInstant nowInstant
    inTenMinutesInstant = case instant (nowMilliseconds <> Milliseconds (Int.toNumber $ 10 * 60 * 1000)) of
      Just i -> i
      Nothing -> unsafeCrashWith "Invalid instant"
  pure $ V1.TimeInterval nowInstant inTenMinutesInstant

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
    V1.TimeInterval ib iha = ctx.timeInterval
    invalidBefore = toDateTime ib
    invalidHereafter = toDateTime iha
    req = PostTransactionsRequest
      { inputs: ctx.inputs
      , invalidBefore
      , invalidHereafter
      , metadata: mempty
      , tags: mempty
      , changeAddress: ctx.wallet.changeAddress
      , addresses: ctx.wallet.usedAddresses
      , collateralUTxOs: []
      }
  post' serverURL transactionsEndpoint req
