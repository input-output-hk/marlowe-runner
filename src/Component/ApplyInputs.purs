module Component.ApplyInputs where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ApplyInputs.Machine (AutoRun(..), InputChoices(..))
import Component.ApplyInputs.Machine as Machine
import Component.BodyLayout (descriptionLink, wrappedContentWithFooter)
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
import Contrib.React.Svg (loadingSpinnerLogo)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), FieldLayout(..), LabelSpacing(..), booleanField, choiceField, intInput, radioFieldChoice, selectFieldChoice)
import Contrib.ReactSyntaxHighlighter (jsonSyntaxHighlighter)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt.Argonaut (toString)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Decimal as Decimal
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

contractSection contract state =
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

type DepositFormComponentProps =
  { depositInputs :: NonEmptyArray DepositInput
  , connectedWallet :: WalletInfo Wallet.Api
  , marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSuccess :: V1.Input -> Effect Unit
  }

mkDepositFormComponent :: MkComponentM (DepositFormComponentProps -> JSX)
mkDepositFormComponent = do
  liftEffect $ component "ApplyInputs.DepositFormComponent" \{ depositInputs, onDismiss, marloweContext, onSuccess } -> React.do
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
        body = fragment $
          [ contractSection marloweContext.contract marloweContext.state
          , DOOM.hr {}
          ] <> [ DOM.div { className: "form-group" } fields ]
        actions = fragment
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
              ]
          ]

      { title: "Select a Deposit to Perform"
      , description: fragment
          [ DOM.p {}
              [ DOOM.text "On the right, you can view all the deposits available for performance at this stage." ]
          , DOM.p {}
              [ DOOM.text "While it's relatively uncommon to have multiple deposit choices at a given point in the contract, it remains a feasible and potentially useful option in certain cases." ]
          ]
      , content: wrappedContentWithFooter body actions
      }

type ChoiceFormComponentProps =
  { choiceInputs :: NonEmptyArray ChoiceInput
  , connectedWallet :: WalletInfo Wallet.Api
  , marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSuccess :: V1.Input -> Effect Unit
  }

mkChoiceFormComponent :: MkComponentM (ChoiceFormComponentProps -> JSX)
mkChoiceFormComponent = do
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.DepositFormComponent" \{ choiceInputs, connectedWallet, marloweContext, onDismiss, onSuccess } -> React.do
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
          [ contractSection marloweContext.contract marloweContext.state
          , DOOM.hr {}
          ] <> [ DOM.div { className: "form-group" } fields ]
        actions = fragment
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
              ]
          ]

      { title: "Apply a Choice"
      , description: DOM.div {}
          [ DOM.p { className: "white-color h5 pb-3" } [ DOOM.text "Select a choice from the dropdown menu to proceed. Each choice represents a decision that a participant can make at a particular point in the contract." ]
          , DOM.p
              { className: "white-color h5 pb-3" }
              [ DOOM.text "Keep in mind that the choices available are defined within the Marlowe contract and may have different consequences or lead to different outcomes. Make sure you understand the implications of each choice before making a selection." ]
          ]
      , content: wrappedContentWithFooter body actions
      }

type NotifyFormComponentProps =
  { notifyInput :: NotifyInput
  , connectedWallet :: WalletInfo Wallet.Api
  , marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSuccess :: Effect Unit
  }

mkNotifyFormComponent :: MkComponentM (NotifyFormComponentProps -> JSX)
mkNotifyFormComponent = do
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.NotifyFormComponent" \{ notifyInput, connectedWallet, marloweContext, onDismiss, onSuccess } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    pure $ BodyLayout.component do
      let
        body = DOOM.div_ $
          [ contractSection marloweContext.contract marloweContext.state
          , DOOM.hr {}
          ]
        actions = fragment
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
                      , onClick: handler_ onSuccess
                      , disabled: false
                      }
                      [ R.text "Submit" ]
                  ]
              ]
          ]

      { title: "Perform a Notify Action"
      , description: DOM.div {}
          [ DOM.p { className: "white-color h5 pb-3" } [ DOOM.text "The Notify action in Marlowe is used to continue the execution of the contract when a certain condition is satisfied. It essentially acts as a gatekeeper, ensuring that the contract only proceeds when the specified observations hold true. " ]
          , DOM.p { className: "white-color h5 pb-3" } [ DOOM.text "This is useful for creating contracts that have conditional flows, where the next steps depend on certain criteria being met. By performing a Notify action, you are signaling that the required conditions are fulfilled and the contract can move forward to the next state." ]
          ]
      , content: wrappedContentWithFooter body actions
      }

type AdvanceFormComponentProps =
  { marloweContext :: Machine.MarloweContext
  , onDismiss :: Effect Unit
  , onSuccess :: Effect Unit
  }

mkAdvanceFormComponent :: MkComponentM (AdvanceFormComponentProps -> JSX)
mkAdvanceFormComponent = do
  liftEffect $ component "ApplyInputs.AdvanceFormComponent" \{ marloweContext, onDismiss, onSuccess } -> React.do
    pure $ BodyLayout.component do
      let
        body = DOOM.div_ $
          [ contractSection marloweContext.contract marloweContext.state
          , DOOM.hr {}
          ]
        actions = fragment
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
                      , onClick: handler_ onSuccess
                      , disabled: false
                      }
                      [ R.text "Submit" ]
                  ]
              ]
          ]
      { title: "Advance the Contract"
      , description: DOM.div {}
          [ DOM.p { className: "white-color h5 pb-5" } [ DOOM.text "Advancing the contract is a crucial action in Marlowe that moves the contract forward to its next state. This action is used when the contract is waiting for something to happen and needs to be manually pushed forward. It's like turning the page to the next chapter in a book. " ]
          , DOM.p { className: "white-color h5 pb-5" } [ DOOM.text "This can be particularly useful in situations where the contract is waiting for an external data feed or an event to occur. By advancing the contract, you are ensuring that the contract stays on course and progresses through its intended sequence of states and actions." ]
          ]
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

-- contractSection :: V1.Contract -> State -> JSX

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
      fields = renderFormSpec autoRunFormSpec formState
      body = fragment $
        [ contractSection contract state
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
      { title: "Apply Input"
      , description: DOM.div {}
          [ DOM.p {} [ DOOM.text "This page allows contract participants to interact with the contract and take actions to progress through it. On the right side of the page, you will see a representation of the contract state as it currently exists on the blockchain. The page is divided into three tabs: Source Graph, Source Code, and Contract State. Each tab provides a different view of the contract." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Source Graph" ]
          , DOM.p {} [ DOOM.text "The Source Graph tab provides a visual representation of the contract. It displays the contract as a graph, with nodes representing the different states and actions of the contract. The paths that have already been executed (transactions) are highlighted, allowing you to see the progression of the contract over time." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Source Code" ]
          , DOM.p {} [ DOOM.text "In the Source Code tab, you can view the remaining part of the contract that is on the blockchain. This includes the logic and conditions that are yet to be executed. It's a textual representation of the contract, allowing you to understand the contract's structure and logic." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Contract State" ]
          , DOM.p {} [ DOOM.text "In the Contract State tab, you can view the current status of the participant's account, as well as the chosen values and variables that have been set within the contract (using 'let' statements). This tab provides a snapshot of the contract's current state and the participant's interaction with it." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Marlowe Explorer" ]
          , DOM.p {} [ DOOM.text "To view the state of the contract on the Cardano blockchain, visit the ", DOM.a { href: "https://preview.marlowescan.com/contractView?tab=info&contractId=09127ec2bd83d20dc108e67fe73f7e40280f6f48ea947606a7b73ac5268985a0%231", target: "_blank", className: "white-color" } [ DOOM.text "Marlowe Explorer" ], DOOM.text "." ]
          ]
      , content: wrappedContentWithFooter body footer
      }

-- In here we want to summarize the initial interaction with the wallet
fetchingRequiredWalletContextDetails marloweContext possibleOnNext onDismiss possibleWalletResponse = do
  let

    statusHtml = DOM.div { className: "row" }
      [ DOM.div { className: "col-12" } $ case possibleWalletResponse of
          Nothing ->
            DOM.div
              { className: "w-100 d-flex justify-content-center align-items-center"
              }
              $ loadingSpinnerLogo
                  {}
          Just walletResponse -> fragment
            [ DOM.p { className: "h3" } $ DOOM.text "Wallet response:"
            , DOM.p {} $ jsonSyntaxHighlighter $ unsafeStringify walletResponse
            ]
      ]

    body = fragment $
      [ contractSection marloweContext.contract marloweContext.state
      , DOOM.hr {}
      ] <> [ statusHtml ]

    footer = DOM.div { className: "row" }
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
                [ R.text "Next" ]
              Just onNext -> DOM.button
                { className: "btn btn-primary"
                , onClick: handler_ onNext
                , disabled: false
                }
                [ R.text "Next" ]
          ]
      ]

  BodyLayout.component
    { title: "Fetching Wallet Context"
    , description:
        DOM.div {}
          [ DOM.p {}
              [ DOOM.text "We are currently fetching the required wallet context for interacting with the contract. This information is essential for confirming your participation in the contract and facilitating the necessary transactions." ]
          , DOM.p {}
              [ DOOM.text "The marlowe-runtime requires information about wallet addresses in order to select the appropriate UTxOs to pay for the initial transaction. To obtain the set of addresses from the wallet, we utilize the "
              , DOM.code {} [ DOOM.text "getUsedAddresses" ]
              , DOOM.text " method from "
              , descriptionLink { label: "CIP-30", href: "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030", icon: "bi-github" }
              ]
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
    { title: "Creating Transaction"
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
    { title: "Signing transaction"
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
    { title: "Submitting transaction signatures"
    , description: fragment
        [ DOM.p {} [ DOOM.text "We are submitting the signatures for the transaction to the Marlowe Runtime now using its REST API." ]
        , DOM.p {} [ DOOM.text "Marlowe Runtime will verify the signatures and if they are correct, it will attach them to the transaction and submit the transaction to the blockchain." ]
        ]
    , content: wrappedContentWithFooter body footer
    }

inputApplied onNext = do
  let
    body = DOM.div { className: "row" }
      [ DOM.div { className: "col-6" }
          [ DOM.p { className: "h3" } $ DOOM.text "Input applied successfully"
          ]
      , DOM.div { className: "col-6" } $ fragment
          [ DOM.p { className: "h3" } $ DOOM.text "API response:"
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
    { title: "Inputs applied successfully"
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

showPossibleErrorAndDismiss title description body onDismiss errors = do
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
    { title
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
      Machine.PresentingContractDetails {} -> do
        let
          onStepSuccess (AutoRun autoRun) = do
            machine.applyAction $ Machine.FetchRequiredWalletContext { autoRun: AutoRun true, marloweContext, transactionsEndpoint }
            traceM "AUTO RUN:"
            traceM autoRun
            if autoRun then setPreviewFlow SimplifiedFlow
            else setPreviewFlow $ DetailedFlow { showPrevStep: true }

        contractDetailsComponent { marloweContext, onSuccess: onStepSuccess, onDismiss }
      Machine.FetchingRequiredWalletContext { errors } -> case previewFlow of
        DetailedFlow _ -> fetchingRequiredWalletContextDetails marloweContext Nothing onDismiss Nothing
        SimplifiedFlow ->
          do
            let
              body = fragment $
                [ contractSection marloweContext.contract marloweContext.state
                , DOOM.hr {}
                ]
            showPossibleErrorAndDismiss "Fetching wallet context" "" body onDismiss errors

      Machine.ChoosingInputType { allInputsChoices, requiredWalletContext } -> case previewFlow of
        DetailedFlow { showPrevStep: true } -> do
          fetchingRequiredWalletContextDetails marloweContext (Just setNextFlow) onDismiss $ Just requiredWalletContext
        _ -> do
          let
            body = fragment $
              [ contractSection marloweContext.contract marloweContext.state
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
              , DOM.div { className: "col-6 text-end" } $
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
            { title: "Select Input Type"
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

      Machine.PickingInput { inputChoices } -> do
        let
          applyPickInputSucceeded input = do
            setNextFlow
            machine.applyAction <<< Machine.PickInputSucceeded $ input
        case inputChoices of
          DepositInputs depositInputs ->
            depositFormComponent { depositInputs, connectedWallet, marloweContext, onDismiss, onSuccess: applyPickInputSucceeded <<< Just }
          ChoiceInputs choiceInputs ->
            choiceFormComponent { choiceInputs, connectedWallet, marloweContext, onDismiss, onSuccess: applyPickInputSucceeded <<< Just }
          SpecificNotifyInput notifyInput ->
            notifyFormComponent { notifyInput, connectedWallet, marloweContext, onDismiss, onSuccess: applyPickInputSucceeded <<< Just $ V1.NormalInput V1.INotify }
          AdvanceContract cont ->
            advanceFormComponent { marloweContext, onDismiss, onSuccess: applyPickInputSucceeded Nothing }
      Machine.CreatingTx { allInputsChoices, errors } -> case previewFlow of
        DetailedFlow _ -> do
          creatingTxDetails Nothing onDismiss "createTx placeholder" $ case errors of
            Just err -> Just $ err
            Nothing -> Nothing
        SimplifiedFlow -> do
          let
            body = DOOM.text "Auto creating tx..."
          showPossibleErrorAndDismiss "Creating Transaction" "" body onDismiss errors
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
          showPossibleErrorAndDismiss "Signing Transaction" "" body onDismiss errors
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
      Machine.InputApplied {} -> case previewFlow of
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
