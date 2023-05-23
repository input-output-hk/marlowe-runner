module Contrib.React.Bootstrap.Modal where

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

foreign import data Fullscreen :: Type

fullscreen
  :: { "lg-down" :: Fullscreen
     , "md-down" :: Fullscreen
     , "sm-down" :: Fullscreen
     , true :: Fullscreen
     , "xl-down" :: Fullscreen
     , "xxl-down" :: Fullscreen
     }
fullscreen =
  { "true": unsafeCoerce true
  , "sm-down": unsafeCoerce "sm-down"
  , "md-down": unsafeCoerce "md-down"
  , "lg-down": unsafeCoerce "lg-down"
  , "xl-down": unsafeCoerce "xl-down"
  , "xxl-down": unsafeCoerce "xxl-down"
  }

foreign import data Size :: Type

size
  :: { lg :: Size
     , sm :: Size
     , xl :: Size
     }
size =
  { "sm": unsafeCoerce "sm"
  , "lg": unsafeCoerce "lg"
  , "xl": unsafeCoerce "xl"
  }

type Props_modal =
  HTMLAttributes +
    ( animation :: Boolean
    , aria_describedby :: String
    , aria_label :: String
    , aria_labelledby :: String
    , autoFocus :: Boolean
    , backdrop :: Backdrop
    , backdropClassName :: String
    , centered :: Boolean
    -- , container :: Any
    , contentClassName :: String
    -- , dialogAs :: ReactComponent
    , dialogClassName :: String
    , enforceFocus :: Boolean
    , fullscreen :: Fullscreen
    , keyboard :: Boolean
    -- A ModalManager instance used to track and manage the state of open Modals. Useful when customizing how modals interact within a container
    -- , manager :: Object
    , onEnter :: Effect Unit
    , onEntered :: Effect Unit
    , onEntering :: Effect Unit
    , onEscapeKeyDown :: Effect Unit
    , onExit :: Effect Unit
    , onExited :: Effect Unit
    , onExiting :: Effect Unit
    , onHide :: Effect Unit
    , onShow :: Effect Unit
    , restoreFocus :: Boolean
    -- , restoreFocusOptions :: Object
    , scrollable :: Boolean
    , show :: Boolean
    , size :: Size
    , bsPrefix :: String
    )

foreign import _Modal :: ReactComponent { | Props_modal }

_internalmodal :: forall attrs attrs_. Row.Union attrs attrs_ Props_modal => ReactComponent { | attrs }
_internalmodal = unsafeCoerce _Modal

modal
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_modal
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
modal props children = element _internalmodal propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_modalBody =
  HTMLAttributes +
    ( -- as :: ReactComponent
      bsPrefix :: String
    )

foreign import _ModalBody :: ReactComponent { | Props_modalBody }

_internalmodalBody :: forall attrs attrs_. Row.Union attrs attrs_ Props_modalBody => ReactComponent { | attrs }
_internalmodalBody = unsafeCoerce _ModalBody

modalBody
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_modalBody
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
modalBody props children = element _internalmodalBody propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_modalDialog =
  HTMLAttributes +
    ( centered :: Boolean
    , contentClassName :: String
    , fullscreen :: Fullscreen
    , scrollable :: Boolean
    , size :: Size
    , bsPrefix :: String
    )

foreign import _ModalDialog :: ReactComponent { | Props_modalDialog }

_internalmodalDialog :: forall attrs attrs_. Row.Union attrs attrs_ Props_modalDialog => ReactComponent { | attrs }
_internalmodalDialog = unsafeCoerce _ModalDialog

modalDialog
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_modalDialog
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
modalDialog props children = element _internalmodalDialog propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_modalFooter =
  HTMLAttributes +
    ( -- as :: ReactComponent
      bsPrefix :: String
    )

foreign import _ModalFooter :: ReactComponent { | Props_modalFooter }

_internalmodalFooter :: forall attrs attrs_. Row.Union attrs attrs_ Props_modalFooter => ReactComponent { | attrs }
_internalmodalFooter = unsafeCoerce _ModalFooter

modalFooter
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_modalFooter
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
modalFooter props children = element _internalmodalFooter propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_modalHeader =
  HTMLAttributes +
    ( closeButton :: Boolean
    , closeLabel :: String
    , closeVariant :: Variant
    , onHide :: Effect Unit
    , bsPrefix :: String
    )

foreign import _ModalHeader :: ReactComponent { | Props_modalHeader }

_internalmodalHeader :: forall attrs attrs_. Row.Union attrs attrs_ Props_modalHeader => ReactComponent { | attrs }
_internalmodalHeader = unsafeCoerce _ModalHeader

modalHeader
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_modalHeader
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
modalHeader props children = element _internalmodalHeader propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props

type Props_modalTitle =
  HTMLAttributes +
    ( -- as :: ReactComponent
      bsPrefix :: String
    )

foreign import _ModalTitle :: ReactComponent { | Props_modalTitle }

_internalmodalTitle :: forall attrs attrs_. Row.Union attrs attrs_ Props_modalTitle => ReactComponent { | attrs }
_internalmodalTitle = unsafeCoerce _ModalTitle

modalTitle
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_modalTitle
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
modalTitle props children = element _internalmodalTitle propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
