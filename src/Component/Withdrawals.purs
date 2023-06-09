module Component.Withdrawals where

import Prelude

import CardanoMultiplatformLib (Bech32)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo)
import Control.Monad.Reader.Class (asks)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (Query)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (snd)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Ada(..)) as V1
import Marlowe.Runtime.Web.Client (post')
import Marlowe.Runtime.Web.Types (PostWithdrawalsRequest(..), PostWithdrawalsResponseContent(..), Runtime(Runtime), TransactionEndpoint, TxOutRef, WithdrawalsEndpoint, txOutRefFromString)
import Polyform.Validator as Validator
import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, useContext, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseForm (useForm)
import React.Basic.Hooks.UseForm as UseForm
import ReactBootstrap.FormBuilder (BootstrapForm, requiredV')
import ReactBootstrap.FormBuilder as FormBuilder
import Wallet as Wallet
import WalletContext (WalletContext(..))

mkWithdrawalForm :: BootstrapForm Effect Query { role :: String, contractId :: TxOutRef }
mkWithdrawalForm = FormBuilder.evalBuilder' ado
  role <- FormBuilder.textInput 
    { label: Just (DOOM.text "Role Name")
    , validator: requiredV' $ Validator.liftFnMaybe (const $ pure "Invalid TxOutRef") Just -- FIXME: no-empty validator
    }
  contractId <- FormBuilder.textInput 
    { label: Just (DOOM.text "Contract Id")
    , validator: requiredV' $ Validator.liftFnMaybe (const $ pure "Invalid TxOutRef") txOutRefFromString
    }
  in
    { role, contractId }

type Props =
  { inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: TransactionEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  , withdrawalsEndpoint :: WithdrawalsEndpoint
  }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "Withdrawal" \{ connectedWallet, onSuccess, onDismiss, inModal, withdrawalsEndpoint } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)

    let
      form = mkWithdrawalForm

      onSubmit :: { result :: _, payload :: _ } -> Effect Unit
      onSubmit = _.result >>> case _, possibleWalletContext of

        Just (V (Right { role, contractId }) /\ _), Just { changeAddress: Just changeAddress, usedAddresses } -> do
          let
            withdrawalContext = WithdrawalContext
              { wallet: { changeAddress, usedAddresses }
              , contractId
              , role
              }
          do
            launchAff_ $ do
              withdrawal withdrawalContext runtime.serverURL runtime.withdrawalsEndpoint >>= case _ of
                -- Right { resource: PostWithdrawalsResponseContent res, links: { transaction: transactionEndpoint } } -> do
                Right { resource: PostWithdrawalsResponseContent res } -> do
                   traceM res
                --  let
                --    { tx } = postContractsResponse
                --    TextEnvelope { cborHex: txCborHex } = tx
                --  let
                --    WalletInfo { wallet: walletApi } = connectedWallet
                --  Wallet.signTx walletApi txCborHex true >>= case _ of
                --    Right witnessSet -> do
                --      submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
                --        Right _ -> do
                --          liftEffect $ onSuccess transactionEndpoint
                --        Left err -> do
                --          traceM "Error while submitting the transaction"
                --          traceM err
                --    Left err -> do
                --      traceM "Failed to sign transaction"
                --      traceM err

                Left err ->
                  traceM $ "Error: " <> show err
            traceM "withdrawal"
            traceM role
            traceM contractId
            pure unit
        _, _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          traceM "withdrawal error"
          pure unit
 
    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }
    pure $ modal
      do
        let
          fields = UseForm.renderForm form formState
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
