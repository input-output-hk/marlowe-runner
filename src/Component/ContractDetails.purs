module Component.ContractDetails where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.InputHelper as InputHelper
import Component.Types (MkComponentM)
import Component.Types.ContractInfo (fetchAppliedInputs)
import Component.Widgets (SpinnerOverlayHeight(..), link, marlowePreview, marloweStatePreview, spinnerOverlay)
import Contrib.React.MarloweGraph (marloweGraph)
import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Tuple.Nested (type (/\))
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (fragment) as DOOM
import React.Basic.DOM (img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, useState', (/\))
import React.Basic.Hooks (bind, discard, useState') as React
import React.Basic.Hooks.Aff (useAff) as React
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
  logger <- asks _.logger
  Runtime.Runtime { serverURL } <- asks _.runtime
  liftEffect $ component "ContractDetails" \{ contract, state, initialState, initialContract, transactionEndpoints, onClose } -> React.do
    possibleExecutionPath /\ setPossibleExecutionPath <- React.useState' Nothing

    React.useAff (transactionEndpoints /\ contract /\ state) do
      fetchAppliedInputs serverURL (Array.reverse transactionEndpoints) >>= case _ of
        V (Right inputs) -> liftEffect $ case InputHelper.executionPath inputs initialContract initialState of
          Right executionPath -> setPossibleExecutionPath executionPath
          Left err -> do
            logger "ContractDetails: failed to compute execution path"
            logger $ unsafeStringify err
        V (Left err) -> liftEffect $ do
          logger "ContractDetails: failed to fetch applied inputs"
          logger $ unsafeStringify err

    graphLoaded /\ setGraphLoaded <- useState' false
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
        tabs { fill: false, justify: false, defaultActiveKey, variant: Tabs.variant.pills } do
          let
            renderTab props children = tab props $ DOM.div { className: "pt-4 h-vh50 d-flex align-items-stretch" } children
          [ case contract of
              Nothing -> mempty
              Just contract' ->
                renderTab
                  { eventKey: eventKey "source"
                  , title: DOOM.span_
                      -- [ Icons.toJSX $ unsafeIcon "filetype-yml"
                      [ DOOM.text " Source code"
                      ]
                  }
                  $ marlowePreview contract'
          , renderTab
              { eventKey: eventKey "graph"
              , title: DOOM.span_
                  -- [ Icons.toJSX $ unsafeIcon "diagram-2"
                  [ DOOM.text " Source graph"
                  ]
              }
              $ marloweGraph
                  { contract: initialContract
                  , executionPath: NoProblem.fromMaybe possibleExecutionPath
                  , onInit: (\_ -> setGraphLoaded true)
                  }
          , case state of
              Nothing -> mempty
              Just st ->
                renderTab
                  { eventKey: eventKey "state"
                  , title: DOOM.span_
                      -- [ Icons.toJSX $ unsafeIcon "bank"
                      [ DOOM.text " Contract state"
                      ]
                  }
                  $ marloweStatePreview st
          ]
      footer = DOOM.fragment
        [ DOM.div
            { className: "col-12 text-center" } $
            [ link
                { label: DOM.b {} [ DOOM.text "Back to contract list" ]
                , onClick: const onClose unit
                , showBorders: false
                , extraClassNames: "mt-3"
                }

            ]
        ]

      content = DOOM.fragment
        [ wrappedContentWithFooter body footer
        , Monoid.guard (not graphLoaded) (spinnerOverlay Spinner100VH)
        ]

    pure $ BodyLayout.component
      { title: DOM.div {}
          [ DOM.div { className: "mb-3" } $ DOOM.img { src: "/images/magnifying_glass.svg" }
          , DOM.span { className: "mb-3" } $ DOOM.text "Contract details"
          ]
      , description: DOM.div { className: "pe-3 mb-3" }
          [ DOM.p {} $ DOOM.text "This page displays the details and current status of the contract that is on chain."
          ]
      , content
      }
