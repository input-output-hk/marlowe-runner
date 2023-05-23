module Contrib.React.Bootstrap.Tab where

import Prelude

import Contrib.React.HTMLAttributes (HTMLAttributes)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type TabPane_props r =
  ( active :: Boolean
  -- , aria-labelledby :: String
  -- , as :: JSX
  , eventKey :: String
  , id :: String
  , mountOnEnter :: Boolean
  , onEnter :: Effect Unit
  , onEntered :: Effect Unit
  , onEntering :: Effect Unit
  , onExit :: Effect Unit
  , onExited :: Effect Unit
  , onExiting :: Effect Unit
  , transition :: Boolean
  , unmountOnExit :: Boolean
  , bsPrefix :: String
  | r
  )

type Props_tab =
  HTMLAttributes
    + TabPane_props
    +
      ( disabled :: Boolean
      -- tabAttrs - Object containing attributes to pass to underlying nav link.
      -- , tabAttrs :: Object
      , tabClassName :: String
      , title :: JSX
      )

foreign import _Tab :: ReactComponent { | Props_tab }

_internaltab :: forall attrs attrs_. Row.Union attrs attrs_ Props_tab => ReactComponent { | attrs }
_internaltab = unsafeCoerce _Tab

tab
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_tab
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
tab props children = element _internaltab propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
