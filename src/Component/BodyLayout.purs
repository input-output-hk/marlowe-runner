module Component.BodyLayout where

import Prelude

import Component.Assets.Svgs (marloweLogoUrl)
import Component.Footer as Footer
import Contrib.React.Svg (svgImg)
import React.Basic.DOM (css)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX)

data BodyContent
  = SimpleContent JSX
  | ContentWithFooter
    { body :: JSX
    , footer :: JSX
    }

-- | At the end it could be stateful because we can make the sidebar collapsible.
component :: { title :: JSX, description :: JSX, content :: BodyContent } -> JSX
component { title, description, content } =
  DOM.div { className: "container-fluid overflow-hidden" } $ do
    let
      footerPlaceholder = DOM.div { style: css { height: "71px" }} ([] :: Array JSX)
      navBarPlaceholder = DOM.div { style: css { height: "59px" }} ([] :: Array JSX)

    DOM.div { className: "row" }
      [ DOM.div { className: "col-3 p-4 background-color-primary white-color overflow-auto vh-100" }
          [ navBarPlaceholder
          , title
          , description
          , footerPlaceholder
          ]
      , DOM.div { className: "col-9 p-0 overflow-auto vh-100" } case content of
          SimpleContent jsx ->
            [ navBarPlaceholder
            , jsx
            , footerPlaceholder
            ]
          ContentWithFooter { body, footer } ->
            [ navBarPlaceholder
            , DOM.div { className: "p-3" } body
            , DOOM.hr {}
            , DOM.div { className: "p-3 text-end" } footer
            , footerPlaceholder
            ]
      ]
