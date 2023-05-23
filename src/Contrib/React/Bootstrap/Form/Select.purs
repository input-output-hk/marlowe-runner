module Contrib.React.Bootstrap.Form.Select where

import Prelude

import Contrib.React.HTMLAttributes (SelectHTMLAttributes)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Size :: Type

size :: { sm :: Size, lg :: Size }
size =
  { sm: unsafeCoerce "sm"
  , lg: unsafeCoerce "lg"
  }

type Props_select =
  SelectHTMLAttributes +
    ( disabled :: Boolean
    , htmlSize :: Int
    , isInvalid :: Boolean
    , isValid :: Boolean
    , onChange :: String -> Effect Unit
    , size :: Size
    , value :: String
    , bsPrefix :: String
    )

foreign import _Select :: ReactComponent { | Props_select }

_internalselect :: forall attrs attrs_. Row.Union attrs attrs_ Props_select => ReactComponent { | attrs }
_internalselect = unsafeCoerce _Select

select
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_select
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
select props children = element _internalselect propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
