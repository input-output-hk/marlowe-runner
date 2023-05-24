module Component.ApplyInputs where

import Prelude

import Actus.Domain (CashFlow)
import Actus.Domain.ContractTerms (ContractTerms)
import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.CreateContract as CreateContract
import Component.InputHelper (ChoiceInput(..), DepositInput(..), NotifyInput(..), nextChoice, nextDeposit)
import Component.Modal (mkModal)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (ActusContractId(..), ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, WalletInfo(..))
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Types.ContractInfo (MarloweInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table (orderingHeader) as Table
import Component.Widgets (link)
import Component.Widgets (link, linkWithIcon)
import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Fetch (FetchError)
import Contrib.Fetch (FetchError)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, ChoiceFieldChoices(..), choiceField, radioFieldChoice, selectFieldChoice)
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, intInput, textInput)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Table (striped) as Table
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Types as OverlayTrigger
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Argonaut.Encode (toJsonString) as Argonaut
import Data.Array (elem, singleton, toUnfoldable)
import Data.Array as Array
import Data.Array as Array
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query (Query(..))
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Newtype (un, unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Time.Duration (Seconds(..))
import Data.Time.Duration as Duration
import Data.Tuple (snd)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Data.Unfoldable (unfoldr1)
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Debug (traceM)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Now (nowDateTime)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Core.V1.Semantics.Types (Case(..), Contract(..), Input(..), InputContent(..), Party, Token)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), InputContent(..), Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Client (post')
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Streaming (TxHeaderWithEndpoint)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent(..), PutContractRequest(PutContractRequest), Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), TxHeader(..), toTextEnvelope)
import Marlowe.Runtime.Web.Types (ContractHeader(..), Metadata, PostTransactionsRequest(..), TxOutRef, txOutRefToString, txOutRefToUrlEncodedString)
import Marlowe.Runtime.Web.Types (PostMerkleizationRequest(..), PostMerkleizationResponse(..), PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), Runtime(..), ServerURL, TextEnvelope(..), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import Marlowe.Runtime.Web.Types as Runtime
import Marlowe.Runtime.Web.Types as Runtime
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither) as Validator
import Polyform.Validator (liftFnMaybe)
import React.Basic (fragment) as DOOM
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM (div_, span_, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as R
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useState, (/\))
import React.Basic.Hooks (JSX, component, useContext, useState', (/\))
import React.Basic.Hooks (JSX, component, useContext, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks as React
import Wallet as Wallet
import Wallet as Wallet
import WalletContext (WalletContext(..))
import WalletContext (WalletContext(..), walletAddresses)

type Result = V1.Contract

data ContractData = ContractData
  { contract :: V1.Contract
  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  -- , collateralUTxOs :: Array TxOutRef
  }

create :: ContractData -> ServerURL -> ContractsEndpoint -> Aff (Either ClientError { resource :: PostContractsResponseContent, links :: {contract :: ContractEndpoint} })
create contractData serverUrl contractsEndpoint = do
  let
    ContractData { contract, changeAddress, usedAddresses } = contractData
    -- metadata = RT.Metadata $ Map.singleton actusMetadataKey $ encodeJson $ Metadata
    --   { contractTerms: contractTerms
    --   , party
    --   , counterParty
    --   }

    req = PostContractsRequest
      { metadata: mempty
      -- , version :: MarloweVersion
      -- , roles :: Maybe RolesConfig
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
        liftFnMaybe (\v -> ["Invalid choice: " <> show v]) \possibleIdx -> do
          idx <- possibleIdx
          Map.lookup idx value2Deposit

      form = FormBuilder.evalBuilder' $
        choiceField { choices, validator }

      onSubmit :: { result :: _ , payload :: _ } -> Effect Unit
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

           -- handler preventDefault \_ -> do
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
        liftFnMaybe (\v -> ["Invalid choice: " <> show v]) \possibleIdx -> do
          idx <- possibleIdx
          Map.lookup idx value2Deposit

      form = FormBuilder.evalBuilder' $ ado
        choice <- choiceField { choices, validator }
        value <- intInput {}
        in
          { choice, value }

      onSubmit :: { result :: _ , payload :: _ } -> Effect Unit
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
      { title: R.text "Perform deposit"
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

data Step
  = Creating CreateInputStep
  | Created (Either String PostContractsResponseContent)
  | Signing (Either String PostContractsResponseContent)
  | Signed (Either ClientError PostContractsResponseContent)


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

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  initialContract <- liftEffect mkInitialContract
  depositFormComponent <- mkDepositFormComponent
  choiceFormComponent <- mkChoiceFormComponent

  liftEffect $ component "ApplyInputs" \{ connectedWallet, onSuccess, onDismiss, contract, state, inModal, timeInterval, transactionsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    step /\ setStep <- useState' (Creating SelectingInputType)
    let
      environment = V1.Environment { timeInterval }

      possibleDeposits = do
        let
          dps = nextDeposit environment state contract
        traceM dps
        NonEmpty.fromArray $ dps

      possibleChoiceInputs = do
        let
          cis = nextChoice environment state contract
        traceM cis
        NonEmpty.fromArray $ cis

    pure $ case step of
      Creating SelectingInputType -> do
        let
          body = DOM.div { className: "row" }
            [ DOM.div { className: "col-12" }
              [ DOM.button
                { className: "btn btn-primary"
                , disabled: isNothing possibleDeposits
                , onClick: handler_ $ case possibleDeposits of
                    Just deposits -> setStep (Creating $ PerformingDeposit deposits)
                    Nothing -> pure unit
                }
                [ R.text "Deposit" ]
              , DOM.button
                { className: "btn btn-primary"
                , disabled: true
                , onClick: handler_ $ pure unit -- setStep (Creating $ PerformingNotify 0)
                }
                [ R.text "Notify" ]
              , DOM.button
                { className: "btn btn-primary"
                , disabled: isNothing possibleChoiceInputs
                , onClick: handler_ $ case possibleChoiceInputs of
                    Just choiceInputs -> setStep (Creating $ PerformingChoice choiceInputs)
                    Nothing -> pure unit
                }
                [ R.text "Choice" ]
              ]
            ]

        if inModal then modal
          { title: R.text "Select input type"
          , onDismiss
          , body
          -- , footer: formActions
          , size: Modal.ExtraLarge
          }
        else
          body
      Creating (PerformingDeposit deposits) -> do
        depositFormComponent { deposits, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
      Creating (PerformingNotify _) -> do
        DOOM.text "NOTIFY"
      Creating (PerformingChoice choiceInputs) -> do
        choiceFormComponent { choiceInputs, connectedWallet, timeInterval, transactionsEndpoint, onDismiss, onSuccess }
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
      [V1.Case
          (V1.Deposit
              (V1.Address address)
              (V1.Address address)
              (V1.Token "" "")
              (V1.Constant $ BigInt.fromInt 1000000)
          )
          V1.Close ]
      timeout
      V1.Close

newtype ApplyInputsContext = ApplyInputsContext
  { wallet :: { changeAddress :: Bech32, usedAddresses :: Array Bech32 }
  , inputs :: Array V1.Input
  , timeInterval :: V1.TimeInterval
  }

applyInputs :: ApplyInputsContext -> ServerURL -> TransactionsEndpoint -> _
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

-- submit
--       >>= case _ of
--         Right ({ resource: PostTransactionsResponse postTransactionsResponse, links: { transaction: transactionEndpoint } }) -> do
--           traceM postTransactionsResponse
--           let
--             { tx } = postTransactionsResponse
--             TextEnvelope { cborHex: txCborHex } = tx
--           Wallet.signTx walletApi txCborHex true >>= case _ of
--             Right witnessSet -> do
--               submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
--                 Right _ -> do
--                   traceM "Successfully submitted the transaction"
--                   -- liftEffect $ msgHubProps.add $ Success $ DOOM.text $ "Successfully submitted a transaction"
--                 -- liftEffect $ onSuccess contractEndpoint
--                 Left err -> do
--                   traceM "Error while submitting the transaction"
--                   -- liftEffect $ msgHubProps.add $ Error $ DOOM.text $ "Error while submitting the transaction"
--                   traceM err
-- 
--             Left err -> do
--               traceM err
--               pure unit
-- 
--           pure unit
--         Left _ -> do
--           traceM token
--           -- traceM $ BigInt.toString value
--           traceM "error"
--           pure unit
-- 
--     pure unit
--   _ -> do
--     -- Note: this happens, when the contract is in status `Unsigned`
--     pure unit
