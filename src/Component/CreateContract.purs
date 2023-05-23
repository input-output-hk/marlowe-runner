module Component.CreateContract where

import Prelude

import CardanoMultiplatformLib (Bech32)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo)
import Component.Widgets (link)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web (Runtime(..))
import Marlowe.Runtime.Web.Client (ClientError)
import Marlowe.Runtime.Web.Types (ContractsEndpoint(..), PostContractsRequest(..), PostContractsResponseContent(..), ServerURL(..), TxOutRef(..))
import Polyform.Validator (liftFnEither) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useContext, useState', (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (WalletContext(..))
import Actus.Domain (ContractTerms(..))
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.CreateContract.Types (FourthStepBaseRow)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Map as Map
import Data.Newtype (class Newtype)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata (Metadata(..), actusMetadataKey)
import Marlowe.Runtime.Web.Client (ClientError(..), merkleize, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent(..), PostMerkleizationRequest(..), PostMerkleizationResponse(..), PutContractRequest(..), Runtime(..), ServerURL, TextEnvelope(..), toTextEnvelope)
import Marlowe.Runtime.Web.Types as RT
import React.Basic (fragment) as DOOM
import React.Basic.DOM (tbody_, td_, text, tr_) as DOOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler)
import React.Basic.Hooks (JSX, component, useState', (/\))
import React.Basic.Hooks as React
import Type.Row (type (+))
import Wallet as Wallet
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows)
import Marlowe.Runtime.Web.Types (TxOutRef)
import Type.Row (type (+))

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

type Result = V1.Contract

mkJsonForm :: _
 -> BootstrapForm Effect Query Result
mkJsonForm cardanoMultiplatformLib = FormBuilder.evalBuilder' $ FormBuilder.textArea
  { missingError: "Please provide contract terms JSON value"
  , helpText: Just $ DOOM.div_
      [ DOOM.text "We gonna perform only a basic JSON validation in here and we won't perform any ACTUS applicablity checks."
      , DOOM.br {}
      , DOOM.text "We implemented a more robust validation schemes in the case of the any other create contract flow than this one."
      ]
  , initial: initialJson
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

  liftEffect $ component "CreateContract" \{ connectedWallet, onSuccess, onDismiss, inModal } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    step /\ setStep <- useState' Creating
    let
      form = mkJsonForm cardanoMultiplatformLib

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


-- "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

initialJson :: String
initialJson = """{"when":[{"then":{"when":[{"then":{"when":[{"then":{"when":[{"then":"close","case":{"for_choice":{"choice_owner":{"role_token":"Buyer"},"choice_name":"Everything is alright"},"choose_between":[{"to":0,"from":0}]}},{"then":{"token":{"token_name":"","currency_symbol":""},"to":{"account":{"role_token":"Buyer"}},"then":{"when":[{"then":"close","case":{"for_choice":{"choice_owner":{"role_token":"Seller"},"choice_name":"Confirm problem"},"choose_between":[{"to":1,"from":1}]}},{"then":{"token":{"token_name":"","currency_symbol":""},"to":{"party":{"address":"addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"}},"then":{"token":{"token_name":"","currency_symbol":""},"to":{"party":{"address":"addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"}},"then":"close","pay":10000000,"from_account":{"role_token":"Buyer"}},"pay":10000000,"from_account":{"role_token":"Seller"}},"case":{"for_choice":{"choice_owner":{"role_token":"Seller"},"choice_name":"Dispute problem"},"choose_between":[{"to":0,"from":0}]}}],"timeout_continuation":"close","timeout":1684853188131},"pay":100000000,"from_account":{"role_token":"Seller"}},"case":{"for_choice":{"choice_owner":{"role_token":"Buyer"},"choice_name":"Report problem"},"choose_between":[{"to":1,"from":1}]}}],"timeout_continuation":"close","timeout":1684852888131},"case":{"party":{"role_token":"Buyer"},"of_token":{"token_name":"","currency_symbol":""},"into_account":{"role_token":"Seller"},"deposits":100000000}}],"timeout_continuation":"close","timeout":1684852588131},"case":{"party":{"role_token":"Buyer"},"of_token":{"token_name":"","currency_symbol":""},"into_account":{"role_token":"Buyer"},"deposits":10000000}}],"timeout_continuation":"close","timeout":1684852288131},"case":{"party":{"role_token":"Seller"},"of_token":{"token_name":"","currency_symbol":""},"into_account":{"role_token":"Seller"},"deposits":10000000}}],"timeout_continuation":"close","timeout":1684851988131}"""
