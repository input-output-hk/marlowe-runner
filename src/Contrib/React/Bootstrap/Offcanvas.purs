module Contrib.React.Bootstrap.Offcanvas where

import Prelude

import Contrib.React.Bootstrap.Types (Backdrop, Variant)
import Contrib.React.HTMLAttributes (HTMLAttributes)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Placement :: Type

placement
  :: { bottom :: Placement
     , end :: Placement
     , start :: Placement
     , top :: Placement
     }
placement =
  { start: unsafeCoerce "start" :: Placement
  , end: unsafeCoerce "end" :: Placement
  , top: unsafeCoerce "top" :: Placement
  , bottom: unsafeCoerce "bottom" :: Placement
  }

foreign import data Responsive :: Type

responsive
  :: { sm :: Responsive
     , md :: Responsive
     , lg :: Responsive
     , xl :: Responsive
     , xxl :: Responsive
     }
responsive =
  { sm: unsafeCoerce "sm" :: Responsive
  , md: unsafeCoerce "md" :: Responsive
  , lg: unsafeCoerce "lg" :: Responsive
  , xl: unsafeCoerce "xl" :: Responsive
  , xxl: unsafeCoerce "xxl" :: Responsive
  }

type Props_offcanvas =
  HTMLAttributes +
    ( "aria-labelledby" :: String
    , autoFocus :: Boolean
    , backdrop :: Backdrop
    , backdropClassName :: String
    , container :: String
    , enforceFocus :: Boolean
    , keyboard :: Boolean
    , onEnter :: Effect Unit
    , onEntered :: Effect Unit
    , onEntering :: Effect Unit
    , onEscapeKeyDown :: Effect Unit
    , onExit :: Effect Unit
    , onExited :: Effect Unit
    , onExiting :: Effect Unit
    , onHide :: Effect Unit
    , onShow :: Effect Unit
    , placement :: Placement
    , renderStaticNode :: Boolean
    , responsive :: Responsive
    , restoreFocus :: Boolean
    , restoreFocusOptions :: String
    , scroll :: Boolean
    , show :: Boolean
    , bsPrefix :: String
    , children :: Array JSX
    )

foreign import _Offcanvas :: ReactComponent { | Props_offcanvas }

_internaloffcanvas :: forall attrs attrs_. Row.Union attrs attrs_ Props_offcanvas => ReactComponent { | attrs }
_internaloffcanvas = unsafeCoerce _Offcanvas

offcanvas
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_offcanvas
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
offcanvas props children = element _internaloffcanvas propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_offcanvasHeader =
  ( closeButton :: Boolean
  , closeLabel :: String
  , closeVariant :: Variant
  , onHide :: Effect Unit
  , bsPrefix :: String
  )

foreign import _OffcanvasHeader :: ReactComponent { | Props_offcanvasHeader }

_internaloffcanvasHeader :: forall attrs attrs_. Row.Union attrs attrs_ Props_offcanvasHeader => ReactComponent { | attrs }
_internaloffcanvasHeader = unsafeCoerce _OffcanvasHeader

offcanvasHeader
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_offcanvasHeader
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
offcanvasHeader props children = element _internaloffcanvasHeader propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_offcanvasTitle =
  ( bsPrefix :: String
  -- as :: String -- elementType; default: <DivStyledAsH5>; You can use a custom element type for this component.
  )

foreign import _OffcanvasTitle :: ReactComponent { | Props_offcanvasTitle }

_internaloffcanvasTitle :: forall attrs attrs_. Row.Union attrs attrs_ Props_offcanvasTitle => ReactComponent { | attrs }
_internaloffcanvasTitle = unsafeCoerce _OffcanvasTitle

offcanvasTitle
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_offcanvasTitle
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
offcanvasTitle props children = element _internaloffcanvasTitle propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_offcanvasBody =
  ( bsPrefix :: String
  -- as :: String -- elementType; default: <div>; You can use a custom element type for this component.
  )

foreign import _OffcanvasBody :: ReactComponent { | Props_offcanvasBody }

_internaloffcanvasBody :: forall attrs attrs_. Row.Union attrs attrs_ Props_offcanvasBody => ReactComponent { | attrs }
_internaloffcanvasBody = unsafeCoerce _OffcanvasBody

offcanvasBody
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_offcanvasBody
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
offcanvasBody props children = element _internaloffcanvasBody propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

