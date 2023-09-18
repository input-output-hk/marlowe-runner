module Component.BodyLayout where

import Prelude

import React.Basic (fragment)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX)

wrappedContentWithFooter :: JSX -> JSX -> JSX
wrappedContentWithFooter body footer = fragment
  [ DOM.div { className: "p-3" } body
  , DOOM.hr {}
  , DOM.div { className: "p-3 mt-auto" } footer
  ]

-- | At the end it could be stateful because we can make the sidebar collapsible.
component :: { title :: JSX, description :: JSX, content :: JSX } -> JSX
component { title, description, content } =
  DOM.div { className: "container-fluid overflow-hidden" } $ do
    DOM.div { className: "row" }
      [ DOM.div { className: "col-3 background-color-primary-light overflow-auto vh-100 px-0 pt-59px pb-71px" } $
          DOM.div { className: "p-3" }
            [ title
            , description
            ]
      , DOM.div { className: "col-9 p-3 m-3 overflow-auto vh-100 pt-59px pb-71px d-flex flex-column" } content
      ]

descriptionLink :: { icon :: String, href :: String, label :: String } -> JSX
descriptionLink { icon, href, label } = DOM.a { href, target: "_blank", className: "white-color" } [ DOOM.i { className: "ms-2 me-1 h6 " <> icon }, DOOM.text label ]
