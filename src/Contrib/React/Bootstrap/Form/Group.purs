module Contrib.React.Bootstrap.Form.Group where

import Contrib.React.HTMLAttributes (HTMLAttributes)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type Props_group = HTMLAttributes + (controlId :: String, children :: Array JSX)

foreign import _Group :: ReactComponent { | Props_group }

_internalgroup :: forall attrs attrs_. Row.Union attrs attrs_ Props_group => ReactComponent { | attrs }
_internalgroup = unsafeCoerce _Group

group
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_group
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
group props children = element _internalgroup propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

