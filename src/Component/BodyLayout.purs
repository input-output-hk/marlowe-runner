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
  DOM.div { className: "container" } $ do
    DOM.div { className: "row min-height-100vh d-flex flex-row align-items-stretch no-gutters" } $
      [ DOM.div { className: "pe-3 col-3 background-color-primary-light overflow-auto d-flex flex-column justify-content-center mb-3 pb-3" } $
          [ DOM.div { className: "fw-bold font-size-2rem my-3" } $ title
          , DOM.div { className: "font-size-1rem" } $ description
          ]
      , DOM.div { className: "ps-3 col-9 bg-white position-relative" } content
      ]

descriptionLink :: { icon :: String, href :: String, label :: String } -> JSX
descriptionLink { icon, href, label } = DOM.a { href, target: "_blank", className: "white-color" } [ DOOM.i { className: "ms-2 me-1 h6 " <> icon }, DOOM.text label ]
