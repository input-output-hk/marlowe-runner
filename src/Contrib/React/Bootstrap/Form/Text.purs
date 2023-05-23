module Contrib.React.Bootstrap.Form.Text where

import Contrib.React.HTMLAttributes (HTMLAttributes)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Column :: Type

type Props_text = HTMLAttributes + (muted :: Boolean)

foreign import _Text :: ReactComponent { | Props_text }

_internaltext :: forall attrs attrs_. Row.Union attrs attrs_ Props_text => ReactComponent { | attrs }
_internaltext = unsafeCoerce _Text

text
  :: forall attrsNoChildren attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_text
  => ToJSX jsx
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
text props children = element _internaltext propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
