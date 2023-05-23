module Contrib.React.Bootstrap.Tabs where

import Prelude

import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

-- transition	
-- Transition | false
-- {Fade}	
-- Sets a default animation strategy for all children <TabPane>s.<tbcont
foreign import data Transition :: Type

foreign import data Variant :: Type

variant
  :: { pills :: Variant
     , tabs :: Variant
     }
variant =
  { tabs: unsafeCoerce "tabs"
  , pills: unsafeCoerce "pills"
  }

type Props_tabs =
  ( activeKey :: String
  , children :: Array JSX
  , defaultActiveKey :: String
  , fill :: Boolean
  , id :: String
  , justify :: Boolean
  , mountOnEnter :: Boolean
  , onSelect :: String -> Effect Unit
  , transition :: Transition
  , unmountOnExit :: Boolean
  , variant :: Variant
  )

foreign import _Tabs :: ReactComponent { | Props_tabs }

_internaltabs :: forall attrs attrs_. Row.Union attrs attrs_ Props_tabs => ReactComponent { | attrs }
_internaltabs = unsafeCoerce _Tabs

tabs
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_tabs
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
tabs props children = element _internaltabs propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
