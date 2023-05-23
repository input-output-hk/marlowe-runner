module Contrib.React.Bootstrap.Collapse where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import data Dimension :: Type

dimension :: { height :: Dimension, width :: Dimension }
dimension =
  { height: unsafeCoerce "height" :: Dimension
  , width: unsafeCoerce "width" :: Dimension
  }

-- | TODO: `timeout` seems to be ignored :-(
type Props_collapse =
  ( appear :: Boolean
  , children :: JSX
  , dimension :: Dimension
  -- getDimensionValue
  , "in" :: Boolean
  , mountOnEnter :: Boolean
  , onEnter :: Effect Unit
  , onEntered :: Effect Unit
  , onEntering :: Effect Unit
  , onExit :: Effect Unit
  , onExited :: Effect Unit
  , onExiting :: Effect Unit
  , role :: String
  , timeout :: Milliseconds
  , unmountOnExit :: Boolean
  )

foreign import _Collapse :: ReactComponent { | Props_collapse }

_internalcollapse :: forall attrs attrs_. Row.Union attrs attrs_ Props_collapse => ReactComponent { | attrs }
_internalcollapse = unsafeCoerce _Collapse

collapse
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_
   . Row.Union attrs attrs_ Props_collapse
  => Row.Union (children :: JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> JSX
  -> JSX
collapse props children = element _internalcollapse propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children } props

