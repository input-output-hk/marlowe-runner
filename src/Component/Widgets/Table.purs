module Component.Widget.Table where

import Prelude

import ReactBootstrap.Icons as Icons
import React.Basic (JSX)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)

type Ordering a = { orderBy :: a, orderAsc :: Boolean }

orderingHeader ordering updateOrdering extraClassNames = do
  let
    orderingIcon =
      if ordering.orderAsc then Icons.arrowDownShort
      else Icons.arrowUpShort
    orderingTh label headerOrdering =
      if ordering.orderBy == headerOrdering then DOM.th { className: "text-center " <> extraClassNames }
        [ Icons.toJSX orderingIcon
        , DOM.a
            { href: "#"
            , className: "text-decoration-none text-black text-decoration-underline-hover"
            , onClick: handler_ $ updateOrdering _ { orderAsc = not ordering.orderAsc }
            }
            [ label :: JSX ]
        ]
      else DOM.th { className: "text-center " <> extraClassNames }
        [ DOM.span { className: "invisible" } $ Icons.toJSX orderingIcon
        , DOM.a
            { href: "#"
            , className: "text-decoration-none text-black text-decoration-underline-hover"
            , onClick: handler_ $ updateOrdering _ { orderBy = headerOrdering }
            }
            [ label ]
        ]
  orderingTh
