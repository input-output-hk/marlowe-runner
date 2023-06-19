module Component.BodyLayout where

import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX)

-- | At the end it could be stateful because we can make the sidebar collapsible.
component :: { title :: JSX, description :: JSX, body :: JSX, footer :: JSX } -> JSX
component { title, description, body, footer } = DOM.div {className: "row"}
  [ DOM.div { className: "col-3" }
    [ title
    , description
    ]
  , DOM.div { className: "col-9" }
    [ body
    , footer
    ]
  ]
