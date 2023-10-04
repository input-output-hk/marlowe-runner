module Component.ConnectWallet where

import Prelude

import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Component.Widgets.Form (mkSingleChoiceField)
import Component.Widgets.Form as Form
import Contrib.React.Svg (loadingSpinnerLogo)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Newtype as Newtype
import Data.String.Extra as String
import Data.Traversable (traverse)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import React.Basic (JSX, fragment)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (button, img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import ReactBootstrap.Modal (modal, modalBody, modalFooter, modalHeader)
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet (Wallet)
import Wallet as Wallet
import Web.HTML (window)

type Wallets = Map String Wallet

walletInfo :: Wallet -> Aff (WalletInfo Wallet)
walletInfo wallet = WalletInfo <$> ado
  name <- liftEffect (Wallet.name wallet)
  icon <- liftEffect (Wallet.icon wallet)
  isEnabled <- Wallet.isEnabled_ wallet
  apiVersion <- liftEffect (Wallet.apiVersion wallet)
  in
    { name, icon, isEnabled, apiVersion, wallet }

data Response
  = NoWallets
  | ConnectionError Error
  | Connected (WalletInfo Wallet.Api)

type Props =
  { currentlyConnected :: Maybe (WalletInfo Wallet.Api)
  , onDismiss :: Effect Unit
  , onWalletConnect :: Response -> Effect Unit
  , inModal :: Boolean
  }

formatName :: String -> String
formatName "GeroWallet" = "GeroWallet"
formatName name = String.upperCaseFirst name

renderWallets :: Effect Unit -> WalletInfo Wallet -> JSX
renderWallets onSubmit (WalletInfo { icon, name }) =
  DOM.div { className: "row mt-2" }
    [ DOM.div { className: "col-12 d-flex rounded p-2 align-items-center border border-2 border-secondary justify-content-between cursor-pointer", onClick: handler_ onSubmit }
        [ DOOM.img { src: icon, alt: "Icon Before", className: "icon" }
        , DOM.div { className: "fw-bold text-start flex-grow-2 ps-2" } $ DOOM.text $ formatName name
        , DOM.div { className: "cardano-badge flex-8" }
            [ DOOM.img { src: "images/cardano-logo.png", alt: "Icon After", className: "icon-after" }
            , DOOM.text "Cardano"
            ]
        ]
    ]

mkConnectWallet :: MkComponentM (Props -> JSX)
mkConnectWallet = do
  singleChoiceField <- liftEffect mkSingleChoiceField
  -- modal <- liftEffect mkModal

  liftEffect $ component "Wallet" \{ currentlyConnected, onWalletConnect, onDismiss, inModal } -> React.do
    -- pure \{ currentlyConnected, onWalletConnect, onDismiss } -> coerceHook React.do
    possibleWallets /\ setWallets <- useState' (Nothing :: Maybe (ArrayAL 1 (WalletInfo Wallet)))
    selectedWallet /\ setSelectedWallet <- useState' $ Nothing

    useEffectOnce do
      liftEffect (Wallet.cardano =<< window) >>= case _ of
        Nothing -> pure unit
        Just cardano -> launchAff_ do
          eternl <- liftEffect (Wallet.eternl cardano) >>= traverse walletInfo
          gerowallet <- liftEffect (Wallet.gerowallet cardano) >>= traverse walletInfo
          lace <- liftEffect (Wallet.lace cardano) >>= traverse walletInfo
          nami <- liftEffect (Wallet.nami cardano) >>= traverse walletInfo
          yoroi <- liftEffect (Wallet.yoroi cardano) >>= traverse walletInfo
          case ArrayAL.fromArray (Proxy :: Proxy 1) (Array.catMaybes [ lace, nami, gerowallet, yoroi, eternl ]) of
            Nothing -> liftEffect $ onWalletConnect NoWallets
            Just wallets -> liftEffect $ do
              setWallets (Just wallets)
              setSelectedWallet $ do
                { name } <- un WalletInfo <$> currentlyConnected
                Array.find (\(WalletInfo wallet) -> wallet.name == name) (ArrayAL.toArray wallets)
      pure (pure unit)

    let
      submit w = case w of
        Just selected@(WalletInfo s) ->
          if Just s.name == (_.name <<< unwrap <$> currentlyConnected) then onDismiss
          else launchAff_ do
            possibleApi <- (Just <$> Wallet.enable_ s.wallet) `catchError` \error -> do
              liftEffect $ onWalletConnect (ConnectionError error)
              pure Nothing
            case possibleApi of
              Just (walletApi :: Wallet.Api) -> do
                let
                  selected' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) selected
                liftEffect $ onWalletConnect (Connected selected')
              Nothing -> do
                traceM $ "Error connecting wallet - no api returned"
                -- FIXME: Error handling
                liftEffect $ onDismiss
        Nothing -> onDismiss
      onSubmit = submit selectedWallet

    pure $ do
      let
        { formBody, formActions } = case possibleWallets of
          Nothing ->
            { formBody: DOM.div { className: "d-flex justify-content-center" } $ loadingSpinnerLogo {}
            , formActions: mempty
            }
          Just wallets -> do
            let
              choices = wallets <#> \(WalletInfo { icon, name }) -> do
                let
                  label = DOM.span { className: "h5" }
                    [ DOOM.img { src: icon, alt: name, className: "w-2rem me-2" }
                    , DOOM.span_ [ DOOM.text name ]
                    ]
                -- We know that only Nami is working - should we disable all the other wallets?
                name /\ label /\ false

            { formBody: singleChoiceField
                { initialValue: fromMaybe "" (_.name <<< unwrap <$> selectedWallet)
                , onValueChange: \walletName -> do
                    setSelectedWallet $ Array.find (\(WalletInfo wallet) -> wallet.name == walletName) (ArrayAL.toArray wallets)
                , type: Form.RadioButtonField choices
                }
            , formActions: DOOM.fragment
                if inModal then
                  [ link { label: DOOM.text "Cancel", onClick: onDismiss, showBorders: true }
                  , DOOM.button do
                      let
                        _name :: forall wallet. Maybe (WalletInfo wallet) -> Maybe String
                        _name = map $ (_.name <<< unwrap)
                        selectedIsConnected = _name selectedWallet == _name currentlyConnected

                      { type: "button"
                      , className: "btn btn-primary"
                      , onClick: handler_ onSubmit
                      , disabled: selectedIsConnected
                      , children: [ DOOM.text "Connect wallet" ]
                      }
                  ]
                else
                  [ DOOM.button do
                      let
                        _name :: forall wallet. Maybe (WalletInfo wallet) -> Maybe String
                        _name = map $ (_.name <<< unwrap)
                        selectedIsConnected = _name selectedWallet == _name currentlyConnected

                      { type: "button"
                      , className: "btn btn-primary mt-3"
                      , onClick: handler_ onSubmit
                      , disabled: selectedIsConnected
                      , children: [ DOM.p { className: "h4 font-weight-bold" } [ DOOM.text "Connect wallet" ] ]
                      }
                  ]
            }
      if inModal then modal
        { onHide: onDismiss -- : setConfiguringWallet false
        -- , footer: formActions
        -- , body: formBody
        -- , title: DOOM.text "Connect wallet"
        , show: true
        }
        [ modalHeader {} $ DOOM.text "Choose a wallet"
        , modalBody {} formBody
        , modalFooter {} formActions
        ]

      else
        DOM.div { className: "container" } $ DOM.div { className: "row justify-content-center mt-4" }
          [ DOM.div { className: "col-12" }
              [ DOM.div { className: "shadow-sm rounded p-4" }
                  case possibleWallets of
                    Just wallets ->
                      [ DOM.div { className: "container" }
                          [ DOM.div { className: "row" }
                              [ DOM.div { className: "col-12" }
                                  [ DOM.h5 { className: "card-title font-weight-bold text-left" } [ DOOM.text "Choose a wallet" ]
                                  , DOM.p { className: "card-help-text text-muted text-left" } [ DOOM.text "Please select a wallet to deploy a contract." ]
                                  ]
                              ]
                          , case possibleWallets of
                              Just wallets -> fragment $ (ArrayAL.toArray wallets) <#> \wallet -> do
                                renderWallets (submit $ Just wallet) wallet
                              Nothing -> mempty
                          , DOM.div { className: "row mt-4 d-none" }
                              [ DOM.div { className: "col-6 text-left p-0" } [ DOM.a { href: "#" } [ DOOM.text "Learn more" ] ]
                              , DOM.div { className: "col-6 p-0" } [ DOM.a { href: "#", className: "text-muted text-right text-decoration-none" } [ DOOM.text "I don't have a wallet" ] ]
                              ]
                          ]
                      ]
                    Nothing ->
                      [ DOM.div { className: "container" }
                          [ DOM.div { className: "row" }
                              [ DOM.div { className: "col-12" }
                                  [ DOM.h5 { className: "card-title font-weight-bold text-left mb-3" } [ DOOM.text "Looks like you don't have a wallet extension installed." ]
                                  , DOM.p { className: "card-help-text text-muted text-left" } [ DOOM.text "Please install a cardano wallet extension, such as Lace, Nami or Eternl in order to proceed and start running Marlowe contracts." ]
                                  ]
                              ]
                          ]
                      ]

              ]
          ]
