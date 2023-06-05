module Component.ApplyInputs where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.InputHelper (ChoiceInput(..), DepositInput(..), NotifyInput, nextChoice, nextDeposit, nextNotify, nextTimeoutAdvance)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Fetch (FetchError)
import Contrib.Language.Marlowe.Core.V1 (compareMarloweJsonKeys)
import React.Basic.Hooks.UseForm (useForm)
import React.Basic.Hooks.UseForm as UseForm
import ReactBootstrap.FormBuilder (ChoiceFieldChoices(SelectFieldChoices, RadioButtonFieldChoices), choiceField, intInput, radioFieldChoice, selectFieldChoice)
import ReactBootstrap.FormBuilder as FormBuilder
import Contrib.ReactSyntaxHighlighter (yamlSyntaxHighlighter)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import JsYaml as JsYaml
import Language.Marlowe.Core.V1.Semantics.Types (Input(..), InputContent(..))
import Language.Marlowe.Core.V1.Semantics.Types (Action(..), Ada(..), Case(..), ChoiceId(..), Contract(..), Environment(..), Input, Party(..), State, TimeInterval(..), Token(..), Value(..)) as V1
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent, PostTransactionsRequest(PostTransactionsRequest), PostTransactionsResponse(PostTransactionsResponse), PutTransactionRequest(PutTransactionRequest), Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnMMaybe, liftFnMaybe)
import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (JSX, component, useContext, useState', (/\))
import React.Basic.Hooks as React
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
  { deposits :: NonEmptyArray DepositInput
  , connectedWallet :: WalletInfo Wallet.Api
  , onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , timeInterval :: V1.TimeInterval
  , transactionsEndpoint :: TransactionsEndpoint
  }

mkDepositFormComponent :: MkComponentM (DepositFormComponentProps -> JSX)
mkDepositFormComponent = do
  modal <- liftEffect mkModal
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.DepositFormComponent" \{ deposits, connectedWallet, onDismiss, onSuccess, timeInterval, transactionsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    -- type ChoiceFieldProps validatorM a =
    --   { choices :: ChoiceFieldChoices
    --   , validator :: Batteries.Validator validatorM String (Maybe String) a
    --   | ChoiceFieldOptionalPropsRow ()
    --   }
    let
      choices = RadioButtonFieldChoices do
        let
          toChoice idx (DepositInput account party token value cont) = do
            let
              label = show value
            radioFieldChoice (show idx) (DOOM.text label)
        { switch: true
        , choices: ArrayAL.fromNonEmptyArray $ mapWithIndex toChoice deposits
        }

      validator :: Batteries.Validator Effect _ _ _
      validator = do
        let
          value2Deposit = Map.fromFoldable $ mapWithIndexFlipped deposits \idx deposit -> show idx /\ deposit
        liftFnMaybe (\v -> [ "Invalid choice: " <> show v ]) \possibleIdx -> do
          idx <- possibleIdx
          Map.lookup idx value2Deposit

      form = FormBuilder.evalBuilder' $
        choiceField { choices, validator }

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _, possibleWalletContext of
        Just (V (Right deposit) /\ _), Just { changeAddress: Just changeAddress, usedAddresses } -> do
          let
            DepositInput account party token value cont = deposit
            inputs = Array.singleton $ NormalInput (IDeposit party party token value)

            applyInputsContext = ApplyInputsContext
              { inputs
              , wallet: { changeAddress, usedAddresses }
              , timeInterval
              }
          do
            launchAff_ $ do
              applyInputs applyInputsContext runtime.serverURL transactionsEndpoint >>= case _ of
                Right { resource: PostTransactionsResponse postContractsResponse, links: { transaction: transactionEndpoint } } -> do
                  let
                    { tx } = postContractsResponse
                    TextEnvelope { cborHex: txCborHex } = tx
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  Wallet.signTx walletApi txCborHex true >>= case _ of
                    Right witnessSet -> do
                      submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                        Right _ -> do
                          liftEffect $ onSuccess transactionEndpoint
                        Left err -> do
                          traceM "Error while submitting the transaction"
                          traceM err
                    Left err -> do
                      traceM "Failed to sign transaction"
                      traceM err

                Left err ->
                  traceM $ "Error: " <> show err
        _, _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }
    pure $ modal do
      let
        fields = UseForm.renderForm form formState
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
      { title: R.text "Perform deposit"
      , body
      , footer: actions
      , onDismiss
      , size: Modal.ExtraLarge
      }

type ChoiceFormComponentProps =
  { choiceInputs :: NonEmptyArray ChoiceInput
  , connectedWallet :: WalletInfo Wallet.Api
  , onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , timeInterval :: V1.TimeInterval
  , transactionsEndpoint :: TransactionsEndpoint
  }

mkChoiceFormComponent :: MkComponentM (ChoiceFormComponentProps -> JSX)
mkChoiceFormComponent = do
  modal <- liftEffect mkModal
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.DepositFormComponent" \{ choiceInputs, connectedWallet, onDismiss, onSuccess, timeInterval, transactionsEndpoint } -> React.do
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

      form = FormBuilder.evalBuilder' $ ado
        choice <- choiceField { choices, validator, touched: true, initial: "0" }
        value <- intInput {}
        in
          { choice, value }

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _, possibleWalletContext of
        Just (V (Right { choice, value }) /\ _), Just { changeAddress: Just changeAddress, usedAddresses } -> do
          let
            ChoiceInput choiceId _ _ = choice
            inputs = Array.singleton $ NormalInput (IChoice choiceId $ BigInt.fromInt value)
            applyInputsContext = ApplyInputsContext
              { inputs
              , wallet: { changeAddress, usedAddresses }
              , timeInterval
              }
          do
            launchAff_ $ do
              applyInputs applyInputsContext runtime.serverURL transactionsEndpoint >>= case _ of
                -- Right res -> do
                --   traceM "APPLY SUCCESS"
                --   traceM res
                Right res@{ resource: PostTransactionsResponse postContractsResponse, links: { transaction: transactionEndpoint } } -> do
                  let
                    { tx } = postContractsResponse
                    TextEnvelope { cborHex: txCborHex } = tx
                  traceM "Successfully created a transaction"
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  Wallet.signTx walletApi txCborHex true >>= case _ of
                    Right witnessSet -> do
                      submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                        Right _ -> do
                          liftEffect $ onSuccess transactionEndpoint
                        Left err -> do
                          traceM "Error while submitting the transaction"
                          traceM err
                    Left err -> do
                      traceM "Failed to sign transaction"
                      traceM err

                Left err ->
                  traceM $ "Error: " <> show err
        _, _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }
    pure $ modal do
      let
        fields = UseForm.renderForm form formState
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
      { title: R.text "Perform choice"
      , body
      , footer: actions
      , onDismiss
      , size: Modal.ExtraLarge
      }

type NotifyFormComponentProps =
  { notifyInputs :: NonEmptyArray NotifyInput
  , connectedWallet :: WalletInfo Wallet.Api
  , onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , timeInterval :: V1.TimeInterval
  , transactionsEndpoint :: TransactionsEndpoint
  }

mkNotifyFormComponent :: MkComponentM (NotifyFormComponentProps -> JSX)
mkNotifyFormComponent = do
  modal <- liftEffect mkModal
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.NotifyFormComponent" \{ notifyInputs, connectedWallet, onDismiss, onSuccess, timeInterval, transactionsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    let
      onSubmit :: EventHandler
      onSubmit = handler_ $ case possibleWalletContext of
        Just { changeAddress: Just changeAddress, usedAddresses } -> do
          let
            inputs = Array.singleton $ NormalInput INotify
            applyInputsContext = ApplyInputsContext
              { inputs
              , wallet: { changeAddress, usedAddresses }
              , timeInterval
              }
          do
            -- FIXME: move aff flow into `useAff` on the component level
            traceM "ON SUBMIT CLICKED"
            launchAff_ $ do
              applyInputs applyInputsContext runtime.serverURL transactionsEndpoint >>= case _ of
                -- Right res -> do
                --   traceM "APPLY SUCCESS"
                --   traceM res
                Right res@{ resource: PostTransactionsResponse postContractsResponse, links: { transaction: transactionEndpoint } } -> do
                  let
                    { contractId, tx } = postContractsResponse
                    TextEnvelope { cborHex: txCborHex } = tx
                    lib = Lib.props cardanoMultiplatformLib
                    txCbor = cborHexToCbor txCborHex
                  traceM "Successfully created a transaction"
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  Wallet.signTx walletApi txCborHex true >>= case _ of
                    Right witnessSet -> do
                      submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                        Right _ -> do
                          liftEffect $ onSuccess transactionEndpoint
                        Left err -> do
                          traceM "Error while submitting the transaction"
                          traceM err
                    Left err -> do
                      traceM "Failed to sign transaction"
                      traceM err

                Left err ->
                  traceM $ "Error: " <> show err
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

    pure $ modal do
      let
        body = DOOM.text ""
        actions = fragment
          [ DOM.button
              { className: "btn btn-primary"
              , onClick: onSubmit
              , disabled: false
              }
              [ R.text "Submit" ]
          ]
      { title: R.text "Perform notify"
      , body
      , footer: actions
      , onDismiss
      , size: Modal.ExtraLarge
      }

type AdvanceFormComponentProps =
  { connectedWallet :: WalletInfo Wallet.Api
  , contract :: V1.Contract
  , onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , timeInterval :: V1.TimeInterval
  , transactionsEndpoint :: TransactionsEndpoint
  }

mkAdvanceFormComponent :: MkComponentM (AdvanceFormComponentProps -> JSX)
mkAdvanceFormComponent = do
  modal <- liftEffect mkModal
  Runtime runtime <- asks _.runtime
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "ApplyInputs.AdvanceFormComponent" \{ contract, connectedWallet, onDismiss, onSuccess, timeInterval, transactionsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    let
      onSubmit :: EventHandler
      onSubmit = handler_ $ case possibleWalletContext of
        Just { changeAddress: Just changeAddress, usedAddresses } -> do
          let
            inputs = []
            applyInputsContext = ApplyInputsContext
              { inputs
              , wallet: { changeAddress, usedAddresses }
              , timeInterval
              }
          do
            -- FIXME: move aff flow into `useAff` on the component level
            launchAff_ $ do
              applyInputs applyInputsContext runtime.serverURL transactionsEndpoint >>= case _ of
                -- Right res -> do
                --   traceM "APPLY SUCCESS"
                --   traceM res
                Right res@{ resource: PostTransactionsResponse postContractsResponse, links: { transaction: transactionEndpoint } } -> do
                  let
                    { contractId, tx } = postContractsResponse
                    TextEnvelope { cborHex: txCborHex } = tx
                    lib = Lib.props cardanoMultiplatformLib
                    txCbor = cborHexToCbor txCborHex
                  traceM "Successfully created a transaction"
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  Wallet.signTx walletApi txCborHex true >>= case _ of
                    Right witnessSet -> do
                      submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                        Right _ -> do
                          liftEffect $ onSuccess transactionEndpoint
                        Left err -> do
                          traceM "Error while submitting the transaction"
                          traceM err
                    Left err -> do
                      traceM "Failed to sign transaction"
                      traceM err

                Left err ->
                  traceM $ "Error: " <> show err
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

    pure $ modal do
      let
        body = DOOM.text ""
        actions = fragment
          [ DOM.button
              { className: "btn btn-primary"
              , onClick: onSubmit
              , disabled: false
              }
              [ R.text "Submit" ]
          ]
      { title: R.text "Advance Contract"
      , body
      , footer: actions
      , onDismiss
      , size: Modal.ExtraLarge
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
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , transactionsEndpoint :: TransactionsEndpoint
  , contract :: V1.Contract
  , state :: V1.State
  , timeInterval :: V1.TimeInterval
  }

sortMarloweKeys :: String -> String -> JsYaml.JsOrdering
sortMarloweKeys a b = JsYaml.toJsOrdering $ compareMarloweJsonKeys a b

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  walletInfoCtx <- asks _.walletInfoCtx

  depositFormComponent <- mkDepositFormComponent
  choiceFormComponent <- mkChoiceFormComponent
  notifyFormComponent <- mkNotifyFormComponent
  advanceFormComponent <- mkAdvanceFormComponent

  liftEffect $ component "ApplyInputs" \{ connectedWallet, onSuccess, onDismiss, contract, state, inModal, timeInterval, transactionsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    step /\ setStep <- useState' (Creating SelectingInputType)
    let
      environment = V1.Environment { timeInterval }

      possibleDeposits = do
        let
          dps = nextDeposit environment state contract
        NonEmpty.fromArray $ dps

      possibleChoiceInputs = do
        let
          cis = nextChoice environment state contract
        NonEmpty.fromArray $ cis

      possibleNotifyInputs = do
        let
          cis = nextNotify environment state contract
        NonEmpty.fromArray $ cis

      possibleNextTimeoutAdvance = nextTimeoutAdvance environment contract

    pure $ case step of
      Creating SelectingInputType -> do
        let
          body = DOM.div { className: "row" }
            [ DOM.div { className: "col-12" } $ yamlSyntaxHighlighter contract { sortKeys: mkFn2 sortMarloweKeys }
            ]

          footer = DOM.div { className: "row" }
            [ DOM.div { className: "col-3 text-center" } $
                DOM.button
                  { className: "btn btn-primary"
                  , disabled: isNothing possibleDeposits || isJust possibleNextTimeoutAdvance
                  , onClick: handler_ $ case possibleDeposits of
                      Just deposits -> setStep (Creating $ PerformingDeposit deposits)
                      Nothing -> pure unit
                  }
                  [ R.text "Deposit" ]
            , DOM.div { className: "col-3 text-center" } $
                DOM.button
                  { className: "btn btn-primary"
                  , disabled: isNothing possibleChoiceInputs || isJust possibleNextTimeoutAdvance
                  , onClick: handler_ $ case possibleChoiceInputs of
                      Just choiceInputs -> setStep (Creating $ PerformingChoice choiceInputs)
                      Nothing -> pure unit
                  }
                  [ R.text "Choice" ]
            , DOM.div { className: "col-3 text-center" } $
                DOM.button
                  { className: "btn btn-primary"
                  , disabled: isNothing possibleNotifyInputs || isJust possibleNextTimeoutAdvance
                  , onClick: handler_ $ case possibleNotifyInputs of
                      Just notifyInputs -> setStep (Creating $ PerformingNotify notifyInputs)
                      Nothing -> pure unit
                  }
                  [ R.text "Notify" ]
            , DOM.div { className: "col-3 text-center" } $
                DOM.button
                  { className: "btn btn-primary"
                  , disabled: isNothing possibleNextTimeoutAdvance
                  , onClick: handler_ $ case possibleNextTimeoutAdvance of
                      Just cont -> setStep (Creating $ PerformingAdvance cont)
                      Nothing -> pure unit
                  }
                  [ R.text "Advance" ]
            ]

        if inModal then modal
          { title: R.text "Select input type"
          , onDismiss
          , body
          , footer
          , size: Modal.ExtraLarge
          }
        else
          body
      Creating (PerformingDeposit deposits) -> do
        depositFormComponent { deposits, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
      Creating (PerformingNotify notifyInputs) -> do
        notifyFormComponent { notifyInputs, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
      Creating (PerformingChoice choiceInputs) -> do
        choiceFormComponent { choiceInputs, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
      Creating (PerformingAdvance cont) -> do
        advanceFormComponent { contract: cont, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
      _ -> DOM.div { className: "row" } [ R.text "TEST" ]

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
