module Component.ContractDetails where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.InputHelper as InputHelper
import Component.MarloweYaml (marloweYaml, marloweStateYaml)
import Component.Types (MkComponentM)
import Component.Types.ContractInfo (fetchAppliedInputs)
import Contrib.React.MarloweGraph (marloweGraph)
import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (_marloweState)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic as DOOM
import React.Basic.DOM (css)
import React.Basic.DOM (span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff as React
import ReactBootstrap.Icons (unsafeIcon)
import ReactBootstrap.Icons as Icons
import ReactBootstrap.Nav (nav)
import ReactBootstrap.Nav as Nav
import ReactBootstrap.Tab (tab)
import ReactBootstrap.Tabs (tabs)
import ReactBootstrap.Tabs as Tabs
import ReactBootstrap.Types (eventKey)

type AppliedInputs = Array (V1.InputContent /\ V1.TimeInterval)

type Props =
  { contract :: Maybe V1.Contract
  , initialContract :: V1.Contract
  , initialState :: V1.State
  , onClose :: Effect Unit
  , state :: Maybe V1.State
  , transactionEndpoints :: Array Runtime.TransactionEndpoint
  }

data ContractView = SourceCode | Graph { compressed :: Boolean }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  Runtime.Runtime { serverURL } <- asks _.runtime
  liftEffect $ component "ContractDetails" \{ contract, state, initialState, initialContract, transactionEndpoints, onClose } -> React.do
    possibleExecutionPath /\ setPossibleExecutionPath <- React.useState' Nothing

    React.useAff (transactionEndpoints /\ contract /\ state) do
      fetchAppliedInputs serverURL (Array.reverse transactionEndpoints) >>= case _ of
        V (Right inputs) -> case InputHelper.executionPath inputs initialContract initialState of
          Right executionPath -> liftEffect $ setPossibleExecutionPath $ Just executionPath
          Left err -> do
            traceM "ContractDetails: failed to compute execution path"
            traceM err
            pure unit
        V (Left err) -> do
          traceM "ContractDetails: failed to fetch applied inputs"
          traceM err
          pure unit

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
      body = do
        let
          defaultActiveKey = case contract of
            Nothing -> "graph"
            Just _ -> "source"
        React.fragment
          [ tabs { fill: true, justify: true, defaultActiveKey, variant: Tabs.variant.pills } do
              let
                renderTab props children = tab props $ DOM.div { className: "pt-4 w-100 h-vh50 overflow-auto"} children
              [ case contract of
                  Nothing -> mempty
                  Just contract' -> renderTab
                    { eventKey: eventKey "source"
                    , title: DOOM.span_
                        [ Icons.toJSX $ unsafeIcon "filetype-yml"
                        , DOOM.text " Source code"
                        ]
                    }
                    [ marloweYaml contract' ]
              , case possibleExecutionPath of
                  Nothing -> mempty
                  Just executionPath ->
                    renderTab
                      { eventKey: eventKey "graph"
                      , title: DOOM.span_
                          [ Icons.toJSX $ unsafeIcon "diagram-2"
                          , DOOM.text " Source graph"
                          ]
                      }
                      [ marloweGraph { contract: initialContract, executionPath } ]
              , case state of
                  Nothing -> mempty
                  Just state -> renderTab
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
      , description: DOM.div {}
          [ DOM.p {} [ DOOM.text "This page shows the details of the selected contract. The page is divided into three tabs: Source Code, Source Graph, and Contract State. Each tab provides a different view of the contract." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Source Code" ]
          , DOM.p {} [ DOOM.text "In the Source Code tab, you can view the remaining part of the contract that is on the blockchain. This includes the logic and conditions that are yet to be executed. It's a textual representation of the contract, allowing you to understand the contract's structure and logic." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Source Graph" ]
          , DOM.p {} [ DOOM.text "The Source Graph tab provides a visual representation of the contract. It displays the contract as a graph, with nodes representing the different states and actions of the contract. The paths that have already been executed (transactions) are highlighted, allowing you to see the progression of the contract over time." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Contract State" ]
          , DOM.p {} [ DOOM.text "In the Contract State tab, you can view the current status of the participant's account, as well as the chosen values and variables that have been set within the contract (using 'let' statements). This tab provides a snapshot of the contract's current state and the participant's interaction with it." ]
          , DOM.p { className: "h3 fw-bold my-3" } [ DOOM.text "Marlowe Explorer" ]
          , DOM.p {} [ DOOM.text "To view the state of the contract on the Cardano blockchain, visit the ", DOM.a { href: "https://preview.marlowescan.com/contractView?tab=info&contractId=09127ec2bd83d20dc108e67fe73f7e40280f6f48ea947606a7b73ac5268985a0%231", target: "_blank", className: "white-color" } [ DOOM.text "Marlowe Explorer" ], DOOM.text "." ]
          ]
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
