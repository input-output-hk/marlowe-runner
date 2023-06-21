module Component.ContractDetails where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.MarloweYaml (marloweYaml, marloweStateYaml)
import Component.Types (MkComponentM)
import Contrib.React.MarloweGraph (marloweGraph)
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (_marloweState)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic as DOOM
import React.Basic.DOM (css)
import React.Basic.DOM (span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, (/\))
import React.Basic.Hooks as React
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Nav (nav)
import ReactBootstrap.Nav as Nav
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)

type Props = { contract :: V1.Contract, onClose :: Effect Unit, state :: V1.State }

data ContractView = SourceCode | Graph { compressed :: Boolean }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractDetails" \{ contract, state, onClose } -> React.do
    contractView /\ setContractView <- React.useState' SourceCode
    -- normalizeContract /\ setNormalizeContract <- React.useState' false

    let
      -- FIXME: We want to present here also:
      --  * `state`,
      --  * link to the explorer,
      --  * switch for contract normalization,
      --  * current contract / history switch,
      -- This a snippet removed from the contract list which was presenting a link to the explorer:
      -- DOM.a
      --  { className: "btn btn-link text-decoration-none text-reset text-decoration-underline-hover truncate-text"
      --  , target: "_blank"
      --  , href: "http://marlowe.palas87.es:8002/contractView?tab=info&contractId=" <> (txOutRefToUrlEncodedString contractId)
      --  }
      sixtyVH = css { "height": "60vh" }
      body = React.fragment
          [ tabs { fill: true, justify: true, defaultActiveKey: "source", variant: Tabs.variant.pills } do
              let
                renderTab props children = tab props $ DOM.div { className: "pt-4 w-100 h-vh50 overflow-auto"} children
              [ renderTab
                { eventKey: eventKey "source"
                , title: DOOM.span_
                    [ Icons.toJSX $ unsafeIcon "filetype-yml"
                    , DOOM.text " Source code"
                    ]
                }
                [ marloweYaml contract ]
              , renderTab
                { eventKey: eventKey "graph"
                , title: DOOM.span_
                    [ Icons.toJSX $ unsafeIcon "diagram-2"
                    , DOOM.text " Source graph"
                    ]
                }
                [ marloweGraph { contract } ]
              , renderTab
                { eventKey: eventKey "state"
                , title: DOOM.span_
                    [ Icons.toJSX $ unsafeIcon "bank"
                    , DOOM.text " Contract state"
                    ]
                }
                [ marloweStateYaml state ]
              ]
          ]
      -- body = nav
      --   { variant: Nav.variant.pills }
      --   [ Nav.link
      --     { eventKey: eventKey "source"}
      --     [ DOOM.span_
      --         [ Icons.toJSX $ unsafeIcon "filetype-yml"
      --         , DOOM.text " Source code"
      --         ]
      --     ]
      --   , Nav.link
      --     { eventKey: eventKey "graph"}
      --     [ DOOM.span_
      --         [ Icons.toJSX $ unsafeIcon "diagram-2"
      --         , DOOM.text " Source graph"
      --         ]
      --     ]
      --   ]
      footer = DOOM.fragment
        [ DOM.button
          { className: "btn btn-primary"
          , onClick: handler_ onClose
          }
          [ DOOM.text "Ok" ]
        ]

      content = wrappedContentWithFooter body footer

    pure $ BodyLayout.component
      { title: "Contract details"
      , description: DOOM.text "View the details of a contract"
      , content
      }

-- <ul class="nav nav-pills">
--   <li class="nav-item">
--     <a class="nav-link active" aria-current="page" href="#">Active</a>
--   </li>
--   <li class="nav-item dropdown">
--     <a class="nav-link dropdown-toggle" data-bs-toggle="dropdown" href="#" role="button" aria-expanded="false">Dropdown</a>
--     <ul class="dropdown-menu">
--       <li><a class="dropdown-item" href="#">Action</a></li>
--       <li><a class="dropdown-item" href="#">Another action</a></li>
--       <li><a class="dropdown-item" href="#">Something else here</a></li>
--       <li><hr class="dropdown-divider"></li>
--       <li><a class="dropdown-item" href="#">Separated link</a></li>
--     </ul>
--   </li>
--   <li class="nav-item">
--     <a class="nav-link" href="#">Link</a>
--   </li>
--   <li class="nav-item">
--     <a class="nav-link disabled">Disabled</a>
--   </li>
-- </ul>
