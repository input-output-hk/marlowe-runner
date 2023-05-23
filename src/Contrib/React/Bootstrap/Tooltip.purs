module Contrib.React.Bootstrap.Tooltip where

import Contrib.React.Bootstrap.Types (Placement)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

type Props_tooltip = (placement :: Placement, show :: Boolean, children :: Array JSX)

foreign import _Tooltip :: ReactComponent { | Props_tooltip }

_internaltooltip :: forall attrs attrs_. Row.Union attrs attrs_ Props_tooltip => ReactComponent { | attrs }
_internaltooltip = unsafeCoerce _Tooltip

tooltip
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_tooltip
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
tooltip props children = element _internaltooltip propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

