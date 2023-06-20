module Component.ContractDetails where

import Prelude

import Component.BodyLayout (BodyContent(..))
import Component.BodyLayout as BodyLayout
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM)
import Contrib.React.MarloweGraph (marloweGraph)
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic as DOOM
import React.Basic.DOM (css)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, (/\))
import React.Basic.Hooks as React
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)

type Props = { contract :: V1.Contract, onClose :: Effect Unit, state :: V1.State }

data ContractView = SourceCode | Graph { compressed :: Boolean }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractDetails" \{ contract, state, onClose } -> React.do
    contractView /\ setContractView <- React.useState' (Graph { compressed: false })
    -- normalizeContract /\ setNormalizeContract <- React.useState' false

    let
      sixtyVH = css { "height": "60vh" }
      body = React.fragment
          [ tabs { fill: true, justify: true, defaultActiveKey: "source", variant: Tabs.variant.pills } do
              let
                renderTab props children = tab props $ DOM.div { className: "row pt-4" } children
              [ renderTab
                { eventKey: eventKey "source"
                , title: DOOM.text "Source code"
                }
                [ DOM.div { className: "child-pre-sixty-vh child-pre-m-0 child-pre-p-0" } [ marloweYaml contract ]]
              , renderTab
                { eventKey: eventKey "graph"
                , title: DOOM.text "Graph"
                }
                [ DOM.div { className: "w-100", style: sixtyVH } [ marloweGraph { contract } ]]
              ]
          ]
      footer = DOOM.fragment
        [ DOM.button
          { className: "btn btn-primary"
          , onClick: handler_ onClose
          }
          [ DOOM.text "Ok" ]
        ]

      content = ContentWithFooter { body, footer }

    pure $ BodyLayout.component
      { title: DOOM.text "Contract details"
      , description: DOOM.text "View the details of a contract"
      , content
      }


