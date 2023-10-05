module Component.Withdrawals where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex)
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (SpinnerOverlayHeight(..), backToContractListLink, spinnerOverlay)
import Contrib.Fetch (FetchError)
import Contrib.Polyform.FormSpecBuilder (evalBuilder)
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), choiceField, radioFieldChoice)
import Control.Monad.Reader.Class (asks)
import Data.Array (filter)
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray, (:))
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for_)
import Data.Validation.Semigroup (V(..))
import Data.Variant (Variant)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types (Ada(..)) as V1
import Marlowe.Runtime.Web.Client (post', put')
import Marlowe.Runtime.Web.Types (Payout(..), PostWithdrawalsRequest(..), PostWithdrawalsResponseContent(..), PutWithdrawalRequest(..), Runtime(Runtime), ServerURL, TextEnvelope(..), TxOutRef, WithdrawalEndpoint, WithdrawalsEndpoint, toTextEnvelope)
import Polyform.Validator (liftFn)
import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, useEffectOnce, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)
import Wallet as Wallet
import WalletContext (WalletContext(..))

type Props =
  { onDismiss :: Effect Unit
  , onSuccess :: WithdrawalEndpoint -> Effect Unit
  , onError :: String -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , roles :: NonEmptyArray String
  , unclaimedPayouts :: Array Payout
  , updateSubmitted :: TxOutRef -> Effect Unit
  , walletContext :: WalletContext
  }

newtype UseSpinnerOverlay = UseSpinnerOverlay Boolean

withdrawalBodyLayout :: UseSpinnerOverlay -> JSX -> JSX
withdrawalBodyLayout (UseSpinnerOverlay useSpinnerOverlay) content = do
  let
    title = DOM.div { className: "" }
      [ DOM.div { className: "mb-3" } $ DOOM.img { src: "/images/magnifying_glass.svg" }
      , DOM.div { className: "mb-3" } $ DOOM.text "Withdraw from the contract"
      ]
    description = DOM.p { className: "mb-3" } $ DOOM.text $ String.joinWith " "
      [ "When payout for a role is created during the contract execution, it is not automatically sent to the wallet but it is locked in a tiny smart contract."
      , "Whoever has the role token in their wallet can withdraw the payout from the contract and use the funds."
      ]
    content' = fragment $
      [ content ]
        <> Monoid.guard useSpinnerOverlay [ spinnerOverlay Spinner100VH ]
  BodyLayout.component { title, description, content: content' }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  liftEffect $ component "Withdrawal" \props@{ connectedWallet, onDismiss, onError, onSuccess, roles, unclaimedPayouts, updateSubmitted } -> React.do
    let
      shouldAutoSubmit = NonEmptyArray.tail roles == []
      shouldUseSpinnerOverlay = UseSpinnerOverlay shouldAutoSubmit

      choices = RadioButtonFieldChoices do
        let toRole idx role = radioFieldChoice (show idx) (DOOM.text role)
        { switch: true
        , choices: ArrayAL.fromNonEmptyArray (mapWithIndex toRole roles)
        }
      WalletContext { changeAddress, usedAddresses } = props.walletContext

      rolesMap = Map.fromFoldableWithIndex roles
      formSpec = evalBuilder Nothing $ ado
        role <- choiceField
          { choices
          , validator: liftFn \idx ->
              fromMaybe "" (idx >>= fromString >>= flip Map.lookup rolesMap)
          }

        in { role }

      onSubmit :: _ -> Effect Unit
      onSubmit = case _ of
        Just (V (Right { role: selectedRole }) /\ _) -> submitRoleWithdrawal selectedRole
        _ -> pure unit

      -- Let's copy the above subsection
      submitRoleWithdrawal selectedRole = do
        let
          payouts = filter (\(Payout { role }) -> role == selectedRole) unclaimedPayouts
          withdrawalContext = WithdrawalContext
            { wallet: { changeAddress, usedAddresses }
            , payouts
            }
        launchAff_ $ do
          withdraw withdrawalContext runtime.serverURL runtime.withdrawalsEndpoint >>= case _ of
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
                      onError $ unsafeStringify err
                Left err -> do
                  onError $ unsafeStringify err
            Left err ->
              onError $ unsafeStringify err
        for_ payouts $ \(Payout { payoutId }) -> updateSubmitted payoutId
        pure unit

    useEffectOnce do
      when shouldAutoSubmit do
        submitRoleWithdrawal $ NonEmptyArray.head roles
      pure $ pure unit

    { formState, onSubmit: onSubmit' } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit: _.result >>> onSubmit
      , validationDebounce: Seconds 0.5
      }

    pure $ do
      let
        fields = StatelessFormSpec.renderFormSpec formSpec formState
        formBody = DOM.div { className: "form-group" } fields
        actions = fragment
          [ DOM.div { className: "row" } $
              [ DOM.div { className: "col-12" } $
                  [ DOM.button
                      do
                        { className: "btn btn-primary w-100"
                        , onClick: onSubmit'
                        }
                      [ R.text "Withdraw"
                      , DOM.span {} $ DOOM.img { src: "/images/arrow_right_alt.svg" }
                      ]
                  ]
              , backToContractListLink onDismiss
              ]
          ]
        content = wrappedContentWithFooter formBody actions
      withdrawalBodyLayout shouldUseSpinnerOverlay content

newtype WithdrawalContext = WithdrawalContext
  { wallet :: { changeAddress :: Bech32, usedAddresses :: Array Bech32 }
  , payouts :: Array Payout
  }

withdraw (WithdrawalContext ctx) serverURL withdrawalsEndpoint = do
  let
    req = PostWithdrawalsRequest
      { payouts: map (\(Payout { payoutId }) -> payoutId) ctx.payouts
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
