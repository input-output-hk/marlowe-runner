module Component.CreateContract where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Contrib.Fetch (FetchError)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Argonaut.Encode (toJsonString) as Argonaut
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (instant, unInstant)
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent(..), PutContractRequest(PutContractRequest), Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), toTextEnvelope)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Validator (liftFnEither) as Validator
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, useContext, useState', (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

type Result = V1.Contract

mkJsonForm :: V1.Contract -> BootstrapForm Effect Query Result
mkJsonForm initialContract = FormBuilder.evalBuilder' $ FormBuilder.textArea
  { missingError: "Please provide contract terms JSON value"
  , helpText: Just $ DOOM.div_
      [ DOOM.text "Basic JSON validation"
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


submit :: CborHex TransactionWitnessSetObject -> ServerURL -> ContractEndpoint -> Aff (Either FetchError Unit)

submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutContractRequest textEnvelope
  put' serverUrl contractEndpoint req

-- FIXME: This is not used yet
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
      form = mkJsonForm initialContract

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
        { title: R.text "Add contract"
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


address :: String
address = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

mkInitialContract :: Effect V1.Contract
mkInitialContract = do
  nowMilliseconds <- unInstant <$> now
  let
    timeout = case instant (nowMilliseconds <> Milliseconds (Int.toNumber $ 20 * 60 * 1000)) of
      Just i -> i
      Nothing -> unsafeCrashWith "Invalid instant"

    zero = BigInt.fromInt 0
    one = BigInt.fromInt 1
    three = BigInt.fromInt 3
    four = BigInt.fromInt 4

  pure $ V1.When
    [ V1.Case
        (V1.Choice
           (V1.ChoiceId "Everything is alright"
              (V1.Address address)) [
           (V1.Bound zero zero)])
        V1.Close
    , V1.Case
        (V1.Choice
           (V1.ChoiceId "Report problem" (V1.Address address))
           [ (V1.Bound one one)])
        V1.Close
    , V1.Case
        (V1.Choice
           (V1.ChoiceId "Choice between 1-3" (V1.Address address))
           [ (V1.Bound one three)])
        V1.Close
    , V1.Case
        (V1.Choice
           (V1.ChoiceId "Choice between 1-4" (V1.Address address))
           [ (V1.Bound one four)])
        V1.Close
    ]
    timeout
    V1.Close
