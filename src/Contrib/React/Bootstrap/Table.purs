module Contrib.React.Bootstrap.Table where

import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import data Responsive :: Type

responsive
  :: { boolean :: Boolean -> Responsive
     , lg :: Responsive
     , md :: Responsive
     , sm :: Responsive
     , xl :: Responsive
     , xs :: Responsive
     }
responsive =
  { xs: unsafeCoerce "xs" :: Responsive
  , sm: unsafeCoerce "sm" :: Responsive
  , md: unsafeCoerce "md" :: Responsive
  , lg: unsafeCoerce "lg" :: Responsive
  , xl: unsafeCoerce "xl" :: Responsive
  , boolean: unsafeCoerce :: Boolean -> Responsive
  }

foreign import data Variant :: Type

variant
  :: { dark :: Variant
     , light :: Variant
     }
variant =
  { dark: unsafeCoerce "dark" :: Variant
  , light: unsafeCoerce "light" :: Variant
  }

foreign import data Size :: Type

sm :: Size
sm = unsafeCoerce "sm"

foreign import data Striped :: Type

striped
  :: { boolean :: Boolean -> Striped
     , columns :: Striped
     }
striped =
  { boolean: unsafeCoerce :: Boolean -> Striped
  , columns: unsafeCoerce "columns" :: Striped
  }

type Props_table =
  ( children :: Array JSX
  , striped :: Striped -- boolean | string
  , bordered :: Boolean -- boolean
  , borderless :: Boolean -- boolean
  , hover :: Boolean -- boolean
  , size :: Size -- string
  , variant :: Variant -- string
  , responsive :: Responsive -- boolean | string
  )

foreign import _Table :: ReactComponent { | Props_table }

_internaltable :: forall attrs attrs_. Row.Union attrs attrs_ Props_table => ReactComponent { | attrs }
_internaltable = unsafeCoerce _Table

table
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_table
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
table props children = element _internaltable propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

