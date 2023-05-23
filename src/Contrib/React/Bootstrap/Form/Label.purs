module Contrib.React.Bootstrap.Form.Label where

import Contrib.React.HTMLAttributes (HTMLAttributes)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Column :: Type

column
  :: { false :: Column
     , lg :: Column
     , sm :: Column
     , true :: Column
     }
column =
  { "true": unsafeCoerce true :: Column
  , "sm": unsafeCoerce "sm" :: Column
  , "lg": unsafeCoerce "lg" :: Column
  , "false": unsafeCoerce false :: Column
  }

type Props_label = HTMLAttributes + (htmlFor :: String, column :: Column)

foreign import _Label :: ReactComponent { | Props_label }

_internallabel :: forall attrs attrs_. Row.Union attrs attrs_ Props_label => ReactComponent { | attrs }
_internallabel = unsafeCoerce _Label

label
  :: forall attrsNoChildren attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_label
  => ToJSX jsx
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
label props children = element _internallabel propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
