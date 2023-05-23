module Component.CreateContract where

import Prelude

import Actus.Domain (ContractTerms(..))
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32)
import CardanoMultiplatformLib (Bech32)
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.CreateContract.Types (FourthStepBaseRow)
import Component.Modal (mkModal)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Types (MkComponentM, WalletInfo)
import Component.Widgets (link)
import Component.Widgets (link)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Argonaut (encodeJson)
import Data.Argonaut as Argonaut
import Data.Argonaut.Encode as Argonaut
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (un)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Debug (traceM)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows)
import Marlowe.Actus.Metadata (Metadata(..), actusMetadataKey)
import Marlowe.Runtime.Web (Runtime(..))
import Marlowe.Runtime.Web.Client (ClientError(..), merkleize, post', put')
import Marlowe.Runtime.Web.Client (ClientError)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent(..), PostMerkleizationRequest(..), PostMerkleizationResponse(..), PutContractRequest(..), Runtime(..), ServerURL, TextEnvelope(..), toTextEnvelope)
import Marlowe.Runtime.Web.Types (ContractsEndpoint(..), PostContractsRequest(..), PostContractsResponseContent(..), ServerURL(..), TxOutRef(..))
import Marlowe.Runtime.Web.Types (TxOutRef)
import Marlowe.Runtime.Web.Types as RT
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Polyform.Validator (liftFnEither) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM (tbody_, td_, text, tr_) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler)
import React.Basic.Hooks (JSX, component, useState', (/\))
import React.Basic.Hooks (component, useContext, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks as React
import Type.Row (type (+))
import Type.Row (type (+))
import Wallet as Wallet
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

type Result = V1.Contract

mkJsonForm :: V1.Contract -> _ -> BootstrapForm Effect Query Result
mkJsonForm initialContract cardanoMultiplatformLib = FormBuilder.evalBuilder' $ FormBuilder.textArea
  { missingError: "Please provide contract terms JSON value"
  , helpText: Just $ DOOM.div_
      [ DOOM.text "We gonna perform only a basic JSON validation in here and we won't perform any ACTUS applicablity checks."
      , DOOM.br {}
      , DOOM.text "We implemented a more robust validation schemes in the case of the any other create contract flow than this one."
      ]
  , initial: Argonaut.toJsonString initialContract
  , validator: requiredV' $ Validator.liftFnEither \jsonString -> do
      json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
      lmap (Array.singleton <<< show) (decodeJson json)
  , rows: 15
  , name: (Just $ FieldId "contract-terms")
  }

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


submit :: CborHex TransactionWitnessSetObject -> ServerURL -> ContractEndpoint -> Aff _
submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutContractRequest textEnvelope
  put' serverUrl contractEndpoint req


data SubmissionStep
  = Creating
  | Created (Either String PostContractsResponseContent)
  | Signing (Either String PostContractsResponseContent)
  | Signed (Either ClientError PostContractsResponseContent)

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  initialContract <- liftEffect mkInitialContract

  liftEffect $ component "CreateContract" \{ connectedWallet, onSuccess, onDismiss, inModal } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    step /\ setStep <- useState' Creating
    let
      form = mkJsonForm initialContract cardanoMultiplatformLib

      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _, possibleWalletContext of
        Just (V (Right contract) /\ _), Just { changeAddress: Just changeAddress, usedAddresses } -> do
          let
            contractData = ContractData
              { contract
              , changeAddress
              , usedAddresses
              }

          -- handler preventDefault \_ -> do
          do
            traceM "ON SUBMIT CLICKED"
            launchAff_ $ do
              create contractData runtime.serverURL runtime.root >>= case _ of
                Right res@{ resource: PostContractsResponseContent postContractsResponse, links: { contract: contractEndpoint } } -> do
                  let
                    { contractId, tx } = postContractsResponse
                    TextEnvelope { cborHex: txCborHex } = tx
                    lib = Lib.props cardanoMultiplatformLib
                    txCbor = cborHexToCbor txCborHex
                  traceM "Successfully created a transaction"
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  Wallet.signTx walletApi txCborHex false >>= case _ of
                    Right witnessSet -> do
                      submit witnessSet runtime.serverURL contractEndpoint >>= case _ of
                        Right _ -> do
                          liftEffect $ onSuccess contractEndpoint
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

    pure $ do
      let
        fields = UseForm.renderForm form formState
        formBody = DOM.div { className: "form-group" } fields
        formActions = DOOM.fragment
          [ link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          , DOM.button
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

      if inModal then modal
        { title: R.text "Add contract | Step 2 of 4"
        , onDismiss
        , body: DOM.div { className: "row" }
            [ DOM.div { className: "col-12" } [ formBody ]
            -- , DOM.div { className: "col-3" } [ DOOM.text "TEST" ]
            ]
        , footer: formActions
        , size: Modal.ExtraLarge
        }
      else
        formBody


address = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

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
