module Contrib.React.Bootstrap.Form.Check where

import Contrib.React.HTMLAttributes (InputHTMLAttributes)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data CheckType :: Type

checkType
  :: { checkbox :: CheckType
     , radio :: CheckType
     , switch :: CheckType
     }
checkType =
  { radio: unsafeCoerce "radio"
  , checkbox: unsafeCoerce "checkbox"
  , switch: unsafeCoerce "switch"
  }

-- | Direct translation from ts to purescript
-- | TODO: Add more handy / safe wrappers.
-- | `type` is only relevant when `as` is an `input`.
type Props_check =
  InputHTMLAttributes +
    ( -- as :: String
      children :: Array JSX
    , disabled :: Boolean
    , feedback :: JSX
    , feedbackTooltip :: Boolean
    , id :: String
    , inline :: Boolean
    , isInvalid :: Boolean
    , isValid :: Boolean
    , label :: JSX
    , reverse :: Boolean
    , title :: String
    , type :: CheckType
    , bsPrefix :: String
    , bsSwitchPrefix :: String
    , value :: String
    )

foreign import _Check :: ReactComponent { | Props_check }

_internalcheck :: forall attrs attrs_. Row.Union attrs attrs_ Props_check => ReactComponent { | attrs }
_internalcheck = unsafeCoerce _Check

check
  :: forall attrs attrs_
   . Row.Union attrs attrs_ Props_check
  => Record attrs
  -> JSX
check props = element _internalcheck props

-- | Check API is strange because it also allows constrution of the children
-- | directly.
checkWithChildren
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_ jsx
   . Row.Union attrs attrs_ Props_check
  => ToJSX jsx
  => Row.Union (children :: Array JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: Array JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> jsx
  -> JSX
checkWithChildren props children = element _internalcheck propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children: toJSX children } props
