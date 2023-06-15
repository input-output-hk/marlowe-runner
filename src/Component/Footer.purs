module Component.Footer where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM as DOM

footer :: JSX
footer =
  DOM.footer
    { className: "footer mt-auto py-3 bg-light"
    , children:
        [ DOM.div
            { className: "container"
            , children:
                [ DOM.span
                    { className: "text-muted"
                    , children: [ DOM.text "Place sticky footer content here." ]
                    }
                ]
            }
        ]
    }
