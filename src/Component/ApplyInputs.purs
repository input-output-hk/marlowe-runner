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
import Component.InputHelper (nextDeposit)
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
import Contrib.Fetch (FetchError)
import Contrib.Fetch (FetchError)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm)
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
import Data.Array.NonEmpty as NonEmpty
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.DateTime.Instant (instant, unInstant)
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query (Query(..))
import Data.Function (on)
import Data.Int as Int
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
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
import Polyform.Validator (liftFnEither) as Validator
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


submit :: CborHex TransactionWitnessSetObject -> ServerURL -> ContractEndpoint -> Aff (Either FetchError Unit)

submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutContractRequest textEnvelope
  put' serverUrl contractEndpoint req


data CreateInputStep
  = SelectingInputType
  | PerformingDeposit Int
  | PerformingNotify Int
  | PerformingChoice Int

data Step
  = Creating CreateInputStep
  | Created (Either String PostContractsResponseContent)
  | Signing (Either String PostContractsResponseContent)
  | Signed (Either ClientError PostContractsResponseContent)

form :: BootstrapForm Effect Query _
form = FormBuilder.evalBuilder' ado
  amount <- intInput {}
  -- party <- textInput { validator: identity }
  -- token <- textInput { validator: identity }
  in
    { amount
    -- , party
    -- , token
    }

--              modal $ do
--                let
--                  fields = UseForm.renderForm form formState
--                  formBody = DOM.div { className: "form-group" } fields
--                  formActions = DOOM.fragment
--                    [
--                    -- link
--                    --     { label: DOOM.text "Cancel"
--                    --     , onClick: onDismiss
--                    --     , showBorders: true
--                    --     }
--                      DOM.button
--                        do
--                          let
--                            disabled = case result of
--                              Just (V (Right _) /\ _) -> false
--                              _ -> true
--                          { className: "btn btn-primary"
--                          , onClick: onSubmit'
--                          , disabled
--                          }
--                        [ R.text "Submit" ]
--                    ]
--
--                { body: DOM.div { className: "row" } [ DOM.div { className: "col-12" } [ formBody ]]
--                , onDismiss: updateState _ { newInput = Nothing }
--                , title: text "Apply inputs"
--                , footer: formActions
--                -- , footer: DOOM.fragment
--                --     [ link
--                --         { label: DOOM.text "Cancel"
--                --         , onClick: updateState _ { newInput = Nothing }
--                --         , showBorders: true
--                --         }
--                --     , DOM.button
--                --         { className: "btn btn-primary"
--                --         , onClick: onApplyInputs input { value: BigInt.fromInt 1, token: V1.Token "" "", party: V1.Address "" } cw
--                --         }
--                --         [ R.text "Submit" ]
--                --     ]
--                }


--     { formState, onSubmit: onSubmit', result } <- useForm
--       { spec: form
--       , onSubmit: \{ result } -> do
--           case result, state.newInput, connectedWallet of
--             Just (V (Right { amount }) /\ _), Just input, Just cw -> do
--               onApplyInputs runtime cardanoMultiplatformLib possibleWalletContext input { value: BigInt.fromInt 1, token: V1.Token "" "", party: V1.Address "" } cw
--               updateState _ { newInput = Nothing }
--             _, _, _ -> pure unit
--       , validationDebounce: Seconds 0.5
--       }

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , transactionsEndpoint :: TransactionsEndpoint
  , contract :: V1.Contract
  , state :: V1.State
  , timeInterval :: V1.TimeInterval
  }

-- newtype MarloweInfo = MarloweInfo
--   { initialContract :: V1.Contract
--   , state :: Maybe V1.State
--   , currentContract :: Maybe V1.Contract
--   }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  initialContract <- liftEffect mkInitialContract

  liftEffect $ component "ApplyInputs" \{ connectedWallet, onSuccess, onDismiss, contract, state, inModal, timeInterval } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    step /\ setStep <- useState' (Creating SelectingInputType)
    let
      environment = V1.Environment { timeInterval }

      --  data TimeInterval = TimeInterval Instant Instant
      --  newtype Environment = Environment { timeInterval :: TimeInterval }
      -- nextDeposit :: Environment -> State -> Contract -> Array DepositInput
      possibleDeposits = NonEmpty.fromArray $ nextDeposit environment state contract

      onSubmit :: _ -> Effect Unit
      onSubmit _ = pure unit
        -- _.result >>> case _, possibleWalletContext of
        --   Just (V (Right contract) /\ _), Just { changeAddress: Just changeAddress, usedAddresses } -> do
        --     let
        --       contractData = ContractData
        --         { contract
        --         , changeAddress
        --         , usedAddresses
        --         }

        --     -- handler preventDefault \_ -> do
        --     do
        --       traceM "ON SUBMIT CLICKED"
        --       launchAff_ $ do
        --         create contractData runtime.serverURL runtime.root >>= case _ of
        --           Right res@{ resource: PostContractsResponseContent postContractsResponse, links: { contract: contractEndpoint } } -> do
        --             let
        --               { contractId, tx } = postContractsResponse
        --               TextEnvelope { cborHex: txCborHex } = tx
        --               lib = Lib.props cardanoMultiplatformLib
        --               txCbor = cborHexToCbor txCborHex
        --             traceM "Successfully created a transaction"
        --             let
        --               WalletInfo { wallet: walletApi } = connectedWallet
        --             Wallet.signTx walletApi txCborHex false >>= case _ of
        --               Right witnessSet -> do
        --                 submit witnessSet runtime.serverURL contractEndpoint >>= case _ of
        --                   Right _ -> do
        --                     liftEffect $ onSuccess contractEndpoint
        --                   Left err -> do
        --                     traceM "Error while submitting the transaction"
        --                     traceM err
        --               Left err -> do
        --                 traceM "Failed to sign transaction"
        --                 traceM err

        --           Left err ->
        --             traceM $ "Error: " <> show err
        --   _, _ -> do
        --     -- Rather improbable path because we disable submit button if the form is invalid
        --     pure unit

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    -- pure $ do
    --   let
    --     fields = UseForm.renderForm form formState
    --     formBody = DOM.div { className: "form-group" } fields
    --     formActions = DOOM.fragment
    --       [ link
    --           { label: DOOM.text "Cancel"
    --           , onClick: onDismiss
    --           , showBorders: true
    --           }
    --       , DOM.button
    --           do
    --             let
    --               disabled = case result of
    --                 Just (V (Right _) /\ _) -> false
    --                 _ -> true
    --             { className: "btn btn-primary"
    --             , onClick: onSubmit'
    --             , disabled
    --             }
    --           [ R.text "Submit" ]
    --       ]

    --   if inModal then modal
    --     { title: R.text "Add contract | Step 2 of 4"
    --     , onDismiss
    --     , body: DOM.div { className: "row" }
    --         [ DOM.div { className: "col-12" } [ formBody ]
    --         -- , DOM.div { className: "col-3" } [ DOOM.text "TEST" ]
    --         ]
    --     , footer: formActions
    --     , size: Modal.ExtraLarge
    --     }
    --   else
    --     formBody
    pure $ case step of
      Creating SelectingInputType -> do
        let
          body = DOM.div { className: "row" }
            [ DOM.div { className: "col-12" }
              [ DOM.button
                { className: "btn btn-primary"
                , disabled: isJust possibleDeposits
                , onClick: handler_ $ setStep (Creating $ PerformingDeposit 0)
                }
                [ R.text "Deposit" ]
              , DOM.button
                { className: "btn btn-primary"
                , disabled: true
                , onClick: handler_ $ setStep (Creating $ PerformingNotify 0)
                }
                [ R.text "Notify" ]
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
