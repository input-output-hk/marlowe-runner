module Contrib.React.Bootstrap.OverlayTrigger where

import Prelude

import Contrib.React.Bootstrap.Types (Placement)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import data OverlayDelay :: Type

overlayDelay
  :: { number :: Number -> OverlayDelay
     , showHide ::
         { hide :: Number
         , show :: Number
         }
         -> OverlayDelay
     }
overlayDelay =
  { number: unsafeCoerce :: Number -> OverlayDelay
  , showHide: unsafeCoerce :: { show :: Number, hide :: Number } -> OverlayDelay
  }

foreign import data OverlayTriggerType :: Type

overlayTriggerType
  :: { click :: OverlayTriggerType
     , focus :: OverlayTriggerType
     , hover :: OverlayTriggerType
     }
overlayTriggerType =
  { hover: unsafeCoerce "hover" :: OverlayTriggerType
  , click: unsafeCoerce "click" :: OverlayTriggerType
  , focus: unsafeCoerce "focus" :: OverlayTriggerType
  }

type Props_overlayTrigger =
  ( children :: Array JSX
  , defaultShow :: Boolean
  , delay :: OverlayDelay
  , flip :: Boolean
  , onToggle :: Boolean -> Effect Unit
  , overlay :: JSX
  , placement :: Placement
  -- , popperConfig
  , trigger :: Array OverlayTriggerType
  )

foreign import _OverlayTrigger :: ReactComponent { | Props_overlayTrigger }

_internalOverlayTrigger :: forall attrs attrs_. Row.Union attrs attrs_ Props_overlayTrigger => ReactComponent { | attrs }
_internalOverlayTrigger = unsafeCoerce _OverlayTrigger

overlayTrigger
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_overlayTrigger
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
overlayTrigger props children = element _internalOverlayTrigger propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
