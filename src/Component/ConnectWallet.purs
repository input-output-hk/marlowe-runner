module Component.ConnectWallet where

import Prelude

import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets as Widgets
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Either (Either(..), fromRight)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.String.Extra as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, on)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import React.Basic (JSX)
import React.Basic.DOM (img, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Record as Record
import Runner.Contrib.Effect.Aff (withTimeout)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Wallet (Wallet)
import Wallet as Wallet
import Web.HTML (window)

type Wallets = Map String Wallet

walletInfo :: Wallet -> Aff (Maybe (WalletInfo Wallet))
walletInfo wallet = withTimeout (Milliseconds 7000.0) $ WalletInfo <$> ado
  name <- liftEffect (Wallet.name wallet)
  icon <- liftEffect (Wallet.icon wallet)
  isEnabled <- Wallet.isEnabled wallet <#> fromRight false
  apiVersion <- liftEffect (Wallet.apiVersion wallet)
  in
    { name, icon, isEnabled, apiVersion, wallet }

-- Version of ApiError without rejection because we handle it using `onDismiss`.
type ApiError' r =
  ( invalidRequest :: String
  , internalError :: String
  , accountChange :: String
  | r
  )

data ConnectionError
  = NoWallets
  | ConnectionError (Variant (ApiError' + Wallet.ApiForeignErrors + Wallet.UnknownError + ()))
  | TimeoutReached

type Result = WalletInfo Wallet.Api

type Props =
  { currentlyConnected :: Maybe (WalletInfo Wallet.Api)
  , onDismiss :: Effect Unit
  , onWalletConnect :: Result -> Effect Unit
  , onError :: ConnectionError -> Effect Unit
  , overlay :: Boolean
  }

formatName :: String -> String
formatName "GeroWallet" = "GeroWallet"
formatName name = String.upperCaseFirst name

renderWallet :: Effect Unit -> WalletInfo Wallet -> JSX
renderWallet onSubmit (WalletInfo { icon, name }) =
  DOM.div { className: "row mt-2" } do
    let
      _aria = Object.fromHomogeneous { labelledby: "button", label: name }
    [ DOM.div
        { className: "col-12 d-flex rounded p-2 align-items-center border border-2 border-secondary justify-content-between cursor-pointer background-color-gray-100-hover"
        , onClick: handler_ onSubmit
        , role: "button"
        , _aria
        }
        [ DOOM.img { src: icon, alt: "Icon Before", className: "icon" }
        , DOM.div { className: "fw-bold text-start flex-grow-2 ps-2" } $ DOOM.text $ formatName name
        , DOM.div { className: "cardano-badge flex-8" }
            [ DOOM.img { src: "images/cardano-logo.png", alt: "Icon After", className: "icon-after" }
            , DOOM.text "Cardano"
            ]
        ]
    ]

data AvailableWallets
  = FetchingWalletList
  | WalletList (ArrayAL 1 (WalletInfo Wallet))
  | NoWalletsAvailable

mkConnectWallet :: MkComponentM (Props -> JSX)
mkConnectWallet = do

  liftEffect $ component "Wallet" \{ currentlyConnected, onWalletConnect, onError, onDismiss, overlay } -> React.do
    possibleWallets /\ setWallets <- useState' FetchingWalletList
    connecting /\ setConnecting <- useState' false

    useEffectOnce do
      liftEffect (Wallet.cardano =<< window) >>= case _ of
        Nothing -> do
          setWallets NoWalletsAvailable
          pure unit
        Just cardano -> launchAff_ do
          eternl <- liftEffect (Wallet.eternl cardano) >>= traverse walletInfo
          gerowallet <- liftEffect (Wallet.gerowallet cardano) >>= traverse walletInfo
          lace <- liftEffect (Wallet.lace cardano) >>= traverse walletInfo
          nami <- liftEffect (Wallet.nami cardano) >>= traverse walletInfo
          yoroi <- liftEffect (Wallet.yoroi cardano) >>= traverse walletInfo
          typhon <- liftEffect (Wallet.typhon cardano) >>= traverse walletInfo
          case ArrayAL.fromArray (Proxy :: Proxy 1) (Array.catMaybes $ map join [ eternl, gerowallet, lace, nami, typhon, yoroi ]) of
            Nothing -> liftEffect $ do
              setWallets NoWalletsAvailable
              onError NoWallets
            Just wallets -> liftEffect $ do
              setWallets $ WalletList wallets
      pure (pure unit)

    let
      submit w = case w of
        Just selected@(WalletInfo s) ->
          if Just s.name == (_.name <<< unwrap <$> currentlyConnected) then onDismiss
          else do
            setConnecting true
            launchAff_ do
              withTimeout (Milliseconds 30000.0) (Wallet.enable s.wallet) >>= case _ of
                Nothing -> liftEffect $ onError TimeoutReached
                Just (Right (walletApi :: Wallet.Api)) -> do
                  let
                    selected' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) selected
                  liftEffect $ onWalletConnect selected'
                Just (Left walletError) -> do
                  let
                    _refused :: Proxy "refused"
                    _refused = Proxy
                    handler =
                      (ConnectionError >>> onError)
                        # on _refused \_ -> liftEffect onDismiss
                  liftEffect $ handler walletError
              liftEffect $ setConnecting false
        Nothing -> onDismiss
      overlayActive = overlay || connecting || case possibleWallets of
        FetchingWalletList -> true
        _ -> false

    -- wrapInLoadingOverlay <- useLoadingOverlay

    pure $ Widgets.renderOverlay { active: overlayActive } $ Array.singleton $ DOM.div { className: "container" } $ DOM.div { className: "row justify-content-center mt-4" }
      [ DOM.div { className: "col-12 shadow-sm rounded p-5 min-height-250px" } do
          let
            renderWallets maybeWallets = do
              [ DOM.div { className: "row" } $
                  DOM.div { className: "col-12" }
                    [ DOM.h5 { className: "card-title font-weight-bold text-left" } [ DOOM.text "Choose a wallet" ]
                    , DOM.p { className: "card-help-text text-muted text-left" } [ DOOM.text "Please select a wallet to deploy a contract." ]
                    ]
              ] <> case maybeWallets of
                Just wallets -> (ArrayAL.toArray wallets) <#> \wallet -> do
                  renderWallet (submit $ Just wallet) wallet
                Nothing -> mempty
          case possibleWallets of
            NoWalletsAvailable ->
              [ DOM.div { className: "row" }
                  [ DOM.div { className: "col-12" }
                      [ DOM.h5 { className: "card-title font-weight-bold text-left mb-3" } [ DOOM.text "Looks like you don't have a wallet extension installed." ]
                      , DOM.p { className: "card-help-text text-muted text-left" } [ DOOM.text "Please install a cardano wallet extension, such as Lace, Nami or Eternl in order to proceed and start running Marlowe contracts." ]
                      ]
                  ]
              ]
            WalletList wallets -> renderWallets (Just wallets)
            _ -> renderWallets Nothing
      ]
