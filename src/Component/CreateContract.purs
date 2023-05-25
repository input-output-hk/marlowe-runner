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
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (asks)
import Control.Promise (Promise, toAff)
import Control.Promise as Promise
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringifyWithIndent)
import Data.Argonaut.Encode (toJsonString) as Argonaut
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (instant, unInstant)
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (snd)
import Data.Typelevel.Undefined (undefined)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign (Foreign)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Client (ClientError, post', put')
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractsEndpoint, PostContractsRequest(..), PostContractsResponseContent(..), PutContractRequest(PutContractRequest), Runtime(Runtime), ServerURL, TextEnvelope(TextEnvelope), toTextEnvelope)
import Partial.Unsafe (unsafeCrashWith)
import Polyform.Validator (liftFnEither) as Validator
import React.Basic (Ref)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text, input) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, readRef, useContext, useRef, useState', (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (WalletContext(..))
import Web.DOM.Node (Node)
import Web.File.File (File)
import Web.File.FileList (FileList)
import Web.File.FileList as FileList
import Web.File.FileReader as FileReader
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement

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
  , initial: stringifyWithIndent 2 $ encodeJson initialContract
  , touched: true
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

create :: ContractData -> ServerURL -> ContractsEndpoint -> Aff (Either ClientError { resource :: PostContractsResponseContent, links :: { contract :: ContractEndpoint } })
create contractData serverUrl contractsEndpoint = do
  let
    ContractData { contract, changeAddress, usedAddresses } = contractData
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

foreign import _loadFile :: File -> Promise (Nullable String)

loadFile :: File -> Aff (Maybe String)
loadFile = map Nullable.toMaybe <<< Promise.toAff <<< _loadFile

hoistMaybe :: forall m a. Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT <<< pure

mkLoadFileButtonComponent :: MkComponentM ({ onFileload :: Maybe String -> Effect Unit } -> JSX)
mkLoadFileButtonComponent =
  liftEffect $ component "LoadFileButton" \{ onFileload } -> React.do
    {- Working example in raw HTML:
      <script>
      const onfile = () => {
        const fr = new FileReader()
        fr.onload = e => {
          console.log("hey ho")
          console.log(e.target.result)
        }
        fr.readAsText(document.getElementById("yo").files[0])
      }
      </script>
      <input id="yo" type="file" onchange="onfile()" />
    -}
    ref :: Ref (Nullable Node) <- useRef Nullable.null

    let
      onChange :: Effect Unit
      onChange = map (fromMaybe unit) $ runMaybeT do
        node :: Node <- MaybeT $ Nullable.toMaybe <$> readRef ref
        inputElement :: HTMLInputElement <- hoistMaybe $ HTMLInputElement.fromNode node
        files :: FileList <- MaybeT $ HTMLInputElement.files inputElement
        file :: File <- hoistMaybe $ FileList.item 0 files
        liftEffect $ launchAff_ $ (liftEffect <<< onFileload) =<< loadFile file

    pure $ DOOM.input { type: "file", onChange: handler_ onChange, ref }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  { multiChoiceTest: initialContract } <- liftEffect $ mkInitialContracts address

  liftEffect $ component "CreateContract" \{ connectedWallet, onSuccess, onDismiss, inModal } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)
    step /\ setStep <- useState' Creating
    let
      form = mkJsonForm brianContract

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

zero = BigInt.fromInt 0
one = BigInt.fromInt 1
three = BigInt.fromInt 3
four = BigInt.fromInt 4

address :: String
address = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

mkInitialContracts :: String -> Effect { multiChoiceTest :: V1.Contract }
mkInitialContracts address = do
  nowMilliseconds <- unInstant <$> now
  let
    timeout = case instant (nowMilliseconds <> Milliseconds (Int.toNumber $ 20 * 60 * 1000)) of
      Just i -> i
      Nothing -> unsafeCrashWith "Invalid instant"

  pure
    { multiChoiceTest: mkMultiChoiceTest address timeout
    }

brianContract = do
  let
    timeout = BigInt.fromString "1684937880000"
    possibleContract = decodeJson $
      encodeJson { "when": [ { "then": { "when": [ { "then": { "when": [ { "then": "close", "case": { "notify_if": true } } ], "timeout_continuation": "close", "timeout": timeout }, "case": { "for_choice": { "choice_owner": { "address": address }, "choice_name": "Release" }, "choose_between": [ { "to": 1, "from": 1 } ] } } ], "timeout_continuation": "close", "timeout": timeout }, "case": { "party": { "address": address }, "of_token": { "token_name": "", "currency_symbol": "" }, "into_account": { "address": address }, "deposits": 10000000 } } ], "timeout_continuation": "close", "timeout": timeout }
  case possibleContract of
    Left err -> unsafeCrashWith $ "Failed to decode contract: " <> show err
    Right contract -> contract

--    , escrow : mkEscrow address
--    }
--
-- mkEscrowWithCollateral :: String -> Int -> Int -> V1.Contract
-- mkEscrowWithCollateral address collateralLovelace priceLovelace = do
--   let
--     collateral = V1.Constant $ BigInt.fromInt collateralLovelace
--     price = V1.Constant $ BigInt.fromInt priceLovelace
--   V1.When [
--     (V1.Case
--        (V1.Deposit
--           (V1.Role "Seller")
--           (V1.Role "Seller")
--           (V1.Token "" "")
--           collateral
--        (V1.When [
--           (V1.Case
--              (V1.Deposit
--                 (V1.Role "Buyer")
--                 (V1.Role "Buyer")
--                 (V1.Token "" "")
--                 collateral
--              (V1.When [
--                 (V1.Case
--                    (V1.Deposit
--                       (V1.Role "Seller")
--                       (V1.Role "Buyer")
--                       (V1.Token "" "")
--                       price)
--                    (V1.When [
--                          (V1.Case
--                             (V1.Choice
--                                (V1.ChoiceId "Everything is alright"
--                                   (V1.Role "Buyer")) [
--                                (V1.Bound 0 0)]) Close)
--                          ,
--                          (V1.Case
--                             (V1.Choice
--                                (V1.ChoiceId "Report problem"
--                                   (V1.Role "Buyer")) [
--                                (V1.Bound 1 1)])
--                             (V1.Pay
--                                (V1.Role "Seller")
--                                (V1.Account
--                                   (V1.Role "Buyer"))
--                                (V1.Token "" "")
--                                price
--                                (V1.When [
--                                      (V1.Case
--                                         (V1.Choice
--                                            (V1.ChoiceId "Confirm problem"
--                                               (V1.Role "Seller")) [
--                                            (V1.Bound 1 1)]) Close)
--                                      ,
--                                      (V1.Case
--                                         (V1.Choice
--                                            (V1.ChoiceId "Dispute problem"
--                                               (V1.Role "Seller")) [
--                                            (V1.Bound 0 0)])
--                                         (V1.Pay
--                                            (V1.Role "Seller")
--                                            (V1.Party
--                                               (V1.PK "0000000000000000000000000000000000000000000000000000000000000000"))
--                                            (V1.Token "" "")
--                                            collateral
--                                            (V1.Pay
--                                               (V1.Role "Buyer")
--                                               (V1.Party
--                                                  (V1.PK "0000000000000000000000000000000000000000000000000000000000000000"))
--                                               (V1.Token "" "")
--                                               (V1.ConstantParam "Collateral amount") Close)))] (TimeParam "Complaint deadline") Close)))] (TimeParam "Dispute by buyer timeout") Close))] (TimeParam "Deposit of price by buyer timeout") Close))] (TimeParam "Deposit of collateral by buyer timeout") Close))] (TimeParam "Collateral deposit by seller timeout") Close

mkMultiChoiceTest :: String -> _ -> V1.Contract
mkMultiChoiceTest address timeout =
  V1.When
    [ V1.Case
        ( V1.Choice
            ( V1.ChoiceId "Everything is alright"
                (V1.Address address)
            )
            [ (V1.Bound zero zero)
            ]
        )
        V1.Close
    , V1.Case
        ( V1.Choice
            (V1.ChoiceId "Report problem" (V1.Address address))
            [ (V1.Bound one one) ]
        )
        V1.Close
    , V1.Case
        ( V1.Choice
            (V1.ChoiceId "Choice between 1-3" (V1.Address address))
            [ (V1.Bound one three) ]
        )
        V1.Close
    , V1.Case
        ( V1.Choice
            (V1.ChoiceId "Choice between 1-4" (V1.Address address))
            [ (V1.Bound one four) ]
        )
        V1.Close
    ]
    timeout
    V1.Close
