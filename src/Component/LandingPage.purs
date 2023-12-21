module Component.LandingPage where

import Prelude

import Component.ConnectWallet (mkConnectWallet)
import Component.ConnectWallet as ConnectWallet
import Component.MessageHub (mkMessageBox)
import Component.Types (ContractInfo, MessageHub(..), MkComponentMBase, WalletInfo)
import Component.Types as MessageHub
import Control.Monad.Reader.Class (asks)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class (liftEffect)
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic.DOM (text, img) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component)
import Type.Row (type (+))
import Wallet as Wallet
import WalletContext (WalletContext)

data DisplayOption = Default | About

derive instance Eq DisplayOption

type ContractInfoMap = Map Runtime.ContractId ContractInfo

-- On the app level we keep the previous wallet context
-- so we can reuse pieces of contract info from the previous
-- state in a safe manner.
newtype AppContractInfoMap = AppContractInfoMap
  { walletContext :: Maybe WalletContext
  , map :: ContractInfoMap
  }

type Props =
  { setWalletInfo :: WalletInfo Wallet.Api -> Effect Unit
  , overlay :: Boolean
  }

data ConnectionErrors
  = NoWallets
  | ConnectionError (Variant (ConnectWallet.ApiError' + Wallet.ApiForeignErrors + Wallet.UnknownError + ()))

mkLandingPage :: MkComponentMBase () (Props -> JSX)
mkLandingPage = do
  connectWallet <- mkConnectWallet
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub
  messageBox <- liftEffect $ mkMessageBox
  liftEffect $ component "LandingPage" \{ setWalletInfo, overlay } -> React.do
    let
      title =
        DOM.div { className: "pe-4 fw-bold" }
          [ DOOM.img { src: "/images/twotone_wallet.svg" }
          , DOM.h3 { className: "fw-bold" } $ DOOM.text "Choose a wallet to deploy a Marlowe smart contract"
          ]
      description =
        DOM.div { className: "pe-4" }
          $ DOM.p {} [ DOOM.text "Selecting a wallet is your first step in deploying a smart contract, your choice should be compatible with the blockchain network you want to deploy your contract on." ]

      content =
        DOM.div { className: "container-fluid" }
          $ DOM.div { className: "row justify-content-center" }
          $ DOM.div { className: "col-xl-7 col-lg-10 col-12" }
          $ connectWallet
              { currentlyConnected: Nothing
              , onWalletConnect: setWalletInfo
              , onError: case _ of
                  ConnectWallet.NoWallets -> pure unit -- setErrors $ Just NoWallets
                  ConnectWallet.ConnectionError _ ->
                    msgHubProps.add $ MessageHub.errorMsg "Wallet connection failed with unknown error. Please try another wallet"
                  ConnectWallet.TimeoutReached ->
                    msgHubProps.add $ MessageHub.errorMsg "Timeout reached while connecting to wallet"
              , onDismiss: pure unit
              , overlay
              }

    pure $ DOM.div { className: "container flex-grow-1 d-flex" } $ do
      [ DOM.div { className: "row flex-grow-1 d-flex flex-row align-items-stretch no-gutters" } $
          [ DOM.div { className: "pe-3 col-3 background-color-primary-light overflow-auto d-flex flex-column justify-content-center mb-3 pb-3" } $
              [ DOM.div { className: "fw-bold font-size-2rem my-3" } $ title
              , DOM.div { className: "font-size-1rem" } $ description
              ]
          , DOM.div { className: "ps-3 col-9 bg-white position-relative" }
              [ DOM.div { className: "mx-5" } $ messageBox msgHub
              , content
              ]
          ]
      ]

