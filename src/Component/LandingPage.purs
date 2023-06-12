module Component.LandingPage where

import Prelude

import Component.ConnectWallet (mkConnectWallet)
import Component.Types (ContractInfo, MkComponentMBase)
import Contrib.React.Svg (SvgUrl(..), svgImg)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX, fragment)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (component)
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
  { routeToApp :: Effect Unit
  }

mkLandingPage :: MkComponentMBase () (Props -> JSX)
mkLandingPage = do
  connectWallet <- mkConnectWallet
  liftEffect $ component "LandingPage" \{ routeToApp } -> React.do
    pure $ DOM.div { className: "mt-6" } $
      [ DOM.div { className: "fixed-top" }
          [ DOM.nav { className: "navbar mb-lg-3 navbar-expand-sm navbar-light bg-light py-0" } $
              DOM.div { className: "container-xl" }
                [ DOM.a { href: "#", className: "navbar-brand" }
                    [ svgImg { src: marloweLogoUrl, height: "30px", className: "me-2" }
                    , DOOM.text "Marlowe Run Lite"
                    ]
                , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                    [ DOM.ul { className: "navbar-nav gap-2" } ([ ] :: Array JSX)
                    ]
                ]
          ]
      , DOM.div { className: "container-xl" }
          $ DOM.div { className: "row" }
              [ DOM.a { href: "#", onClick: handler_ routeToApp } [ DOOM.text "Got to app" ]
              , connectWallet { currentlyConnected: Nothing, onWalletConnect: const $ pure unit, onDismiss: pure unit, inModal: false }
              ]
      ]

marloweLogoUrl :: SvgUrl
marloweLogoUrl = SvgUrl "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iNTAiIGhlaWdodD0iNjAiIHZpZXdCb3g9IjAgMCA1MCA2MCI+CiAgICA8ZGVmcz4KICAgICAgICA8cGF0aCBpZD0idG82NzR1ODZhYSIgZD0iTTAgMEw1MCAwIDUwIDYwIDAgNjB6Ii8+CiAgICA8L2RlZnM+CiAgICA8ZyBmaWxsPSJub25lIiBmaWxsLXJ1bGU9ImV2ZW5vZGQiPgogICAgICAgIDxnPgogICAgICAgICAgICA8ZyB0cmFuc2Zvcm09InRyYW5zbGF0ZSgtODQ5IC0zNjgpIHRyYW5zbGF0ZSg4NDkgMzY4KSI+CiAgICAgICAgICAgICAgICA8bWFzayBpZD0ibDk5N3EzbWw4YiIgZmlsbD0iI2ZmZiI+CiAgICAgICAgICAgICAgICAgICAgPHVzZSB4bGluazpocmVmPSIjdG82NzR1ODZhYSIvPgogICAgICAgICAgICAgICAgPC9tYXNrPgogICAgICAgICAgICAgICAgPHBhdGggZmlsbD0iIzAwRTM5QyIgZD0iTTUuOTEgNjBjLS41NiAwLTEuMTE3LS4wOC0xLjY1OS0uMjM5LTEuNTEzLS40NDMtMi43NjUtMS40NS0zLjUyMi0yLjgzNi0uNzU4LTEuMzg1LS45MzItMi45ODMtLjQ5LTQuNUwxMS4yIDE0Ljg0NmMuMzYtMS4yMzMgMS4wOTItMi4yOTQgMi4xMTctMy4wNjcgMS4wMjQtLjc3MiAyLjI0NC0xLjE4NCAzLjUyNy0xLjE4OWguMDI0YzEuMjc4IDAgMi40OTYuNDA0IDMuNTIyIDEuMTY3IDEuMDI2Ljc2NCAxLjc2MyAxLjgxNiAyLjEzMiAzLjA0M2wyLjM5NyA3Ljk3NyA1LjQxLTE4LjUyM2MuMzYtMS4yMzMgMS4wOTItMi4yOTQgMi4xMTgtMy4wNjZDMzMuNDcyLjQxNiAzNC42OTIuMDA1IDM1Ljk3NCAwaC4wMjJjMS4yNzkgMCAyLjQ5Ny40MDQgMy41MjMgMS4xNjggMS4wMjYuNzY1IDEuNzYzIDEuODE3IDIuMTMyIDMuMDQ1TDQ5Ljc0NyAzMS4yYy40NTQgMS41MTMuMjkzIDMuMTEzLS40NTQgNC41MDQtLjc0NyAxLjM5Mi0xLjk5IDIuNDA4LTMuNTAxIDIuODYzLS41NTQuMTY3LTEuMTI3LjI1Mi0xLjcwMS4yNTItMS4yNDMgMC0yLjQ4My0uNDA3LTMuNDktMS4xNDYtMS4wNDMtLjc2NS0xLjc5LTEuODI2LTIuMTYzLTMuMDY4bC0yLjM2Ni03Ljg4Ny01LjQwNSAxOC41MDZjLS4zNiAxLjIzNC0xLjA5MiAyLjI5NC0yLjExNyAzLjA2Ni0xLjAyNS43NzItMi4yNDQgMS4xODMtMy41MjcgMS4xODhIMjVjLTEuMjc4IDAtMi40OTYtLjQwMy0zLjUyMi0xLjE2Ny0xLjAyNi0uNzY0LTEuNzYzLTEuODE2LTIuMTMyLTMuMDQybC0yLjM5My03Ljk2NS01LjM3OCAxOC40MzhjLS4zNjUgMS4yNTItMS4xMTEgMi4zMjMtMi4xNTcgMy4wOTdDOC40MDcgNTkuNTg4IDcuMTYxIDYwIDUuOTEgNjAiIG1hc2s9InVybCgjbDk5N3EzbWw4YikiLz4KICAgICAgICAgICAgPC9nPgogICAgICAgIDwvZz4KICAgIDwvZz4KPC9zdmc+Cg=="
