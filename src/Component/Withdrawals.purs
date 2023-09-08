module Component.Withdrawals where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder)
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), choiceField, radioFieldChoice)
import Control.Monad.Reader.Class (asks)
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Data.Variant (Variant)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Ada(..)) as V1
import Marlowe.Runtime.Web.Client (post', put')
import Marlowe.Runtime.Web.Types (PostWithdrawalsRequest(..), PostWithdrawalsResponseContent(..), PutWithdrawalRequest(..), Runtime(Runtime), ServerURL, TextEnvelope(..), TxOutRef, WithdrawalEndpoint, WithdrawalsEndpoint, toTextEnvelope)
import Polyform.Validator (liftFn)
import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, useContext, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: WithdrawalEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , withdrawalsEndpoint :: WithdrawalsEndpoint
  , roles :: NonEmptyArray String
  , contractId :: TxOutRef
  }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "Withdrawal" \{ connectedWallet, onSuccess, onDismiss, inModal, roles, contractId, withdrawalsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)

    let
      choices = RadioButtonFieldChoices do
        let toRole idx role = radioFieldChoice (show idx) (DOOM.text role)
        { switch: true
        , choices: ArrayAL.fromNonEmptyArray (mapWithIndex toRole roles)
        }

      rolesMap = Map.fromFoldableWithIndex roles
      formSpec = evalBuilder Nothing $ ado
        role <- choiceField
          { choices
          , validator: liftFn \idx ->
              fromMaybe "" (idx >>= fromString >>= flip Map.lookup rolesMap)
          }

        in { role }

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _, possibleWalletContext of

        Just (V (Right { role }) /\ _), Just { changeAddress, usedAddresses } -> do
          let
            withdrawalContext = WithdrawalContext
              { wallet: { changeAddress, usedAddresses }
              , contractId
              , role
              }
          do
            launchAff_ $ do
              withdrawal withdrawalContext runtime.serverURL runtime.withdrawalsEndpoint >>= case _ of
                Right { resource: PostWithdrawalsResponseContent res, links: { withdrawal: withdrawalEndpoint } } -> do
                  let
                    { tx } = res
                    TextEnvelope { cborHex: txCborHex } = tx
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  Wallet.signTx walletApi txCborHex true >>= case _ of
                    Right witnessSet -> do
                      submit witnessSet runtime.serverURL withdrawalEndpoint >>= case _ of
                        Right _ -> do
                          liftEffect $ onSuccess withdrawalEndpoint
                        Left err -> do
                          traceM "Error while submitting the transaction"
                          traceM err
                    Left err -> do
                      traceM "Failed to sign transaction"
                      traceM err

                Left err ->
                  traceM $ "Error: " <> show err
            traceM "withdrawal"
            -- traceM role
            traceM contractId
            pure unit
        _, _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          traceM "withdrawal error"
          pure unit
 
    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }
    pure $ modal
      do
        let
          fields = StatelessFormSpec.renderFormSpec formSpec formState
          formBody = DOM.div { className: "form-group" } fields
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

        { title: R.text "Withdrawal"
        , onDismiss
        , body: DOM.div { className: "row" }
            [ DOM.div { className: "col-12" } [ formBody ]
            ]
        , footer: actions
        , size: Modal.ExtraLarge
        }

newtype WithdrawalContext = WithdrawalContext
  { wallet :: { changeAddress :: Bech32, usedAddresses :: Array Bech32 }
  , contractId :: TxOutRef
  , role :: String
  }

withdrawal (WithdrawalContext ctx) serverURL withdrawalsEndpoint = do
  let
    req = PostWithdrawalsRequest
      { role: ctx.role
      , contractId: ctx.contractId
      , changeAddress: ctx.wallet.changeAddress
      , addresses: ctx.wallet.usedAddresses
      , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
      , collateralUTxOs: []
      }

  post' @String serverURL (withdrawalsEndpoint :: WithdrawalsEndpoint) req

submit
  :: CborHex TransactionWitnessSetObject
  -> ServerURL
  -> WithdrawalEndpoint
  -> Aff (Either FetchError Unit)
submit witnesses serverUrl contractEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""
    req = PutWithdrawalRequest textEnvelope
  put' serverUrl contractEndpoint req

sign
  :: Wallet.Api
  -> TextEnvelope TransactionObject
  -> Aff
       ( Either
           (Variant (Wallet.SignTxError ()))
           (CborHex TransactionWitnessSetObject)
       )
sign walletApi tx = do
  let
    TextEnvelope { cborHex: txCborHex } = tx
  Wallet.signTx walletApi txCborHex false
