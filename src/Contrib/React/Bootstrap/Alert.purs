module Contrib.React.Bootstrap.Alert where

import Prelude

import Contrib.React.Bootstrap.Types (Variant)
import Contrib.React.HTMLAttributes (HTMLAttributes)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type Props_alert =
  HTMLAttributes +
    (children :: Array JSX, transition :: Boolean, onClose :: Effect Unit, closeLabel :: String, dismissible :: Boolean, show :: Boolean, variant :: Variant)

foreign import _Alert :: ReactComponent { | Props_alert }

_internalalert :: forall attrs attrs_. Row.Union attrs attrs_ Props_alert => ReactComponent { | attrs }
_internalalert = unsafeCoerce _Alert

alert
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_alert
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
alert props children = element _internalalert propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
