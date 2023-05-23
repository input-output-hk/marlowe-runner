module Contrib.React.Bootstrap.Fade where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

-- | TODO: `timeout` seems to be ignored. Use `Collapse` element instead.
type Props_fade =
  ( appear :: Boolean
  , children :: JSX
  , "in" :: Boolean
  , mountOnEnter :: Boolean
  , onEnter :: Effect Unit
  , onEntered :: Effect Unit
  , onEntering :: Effect Unit
  , onExit :: Effect Unit
  , onExited :: Effect Unit
  , onExiting :: Effect Unit
  , timeout :: Milliseconds
  -- , transitionClasses :: { enter :: String, entered :: String, exiting :: String, exited :: String }
  , unmountOnExit :: Boolean
  )

foreign import _Fade :: ReactComponent { | Props_fade }

_internalfade :: forall attrs attrs_. Row.Union attrs attrs_ Props_fade => ReactComponent { | attrs }
_internalfade = unsafeCoerce _Fade

fade
  :: forall attrsNoChildren attrsWithDuplicate attrs attrs_
   . Row.Union attrs attrs_ Props_fade
  => Row.Union (children :: JSX) attrsNoChildren attrsWithDuplicate
  => Row.Nub (children :: JSX | attrsNoChildren) attrs
  => Record attrsNoChildren
  -> JSX
  -> JSX
fade props children = element _internalfade propsWithChildren
  where
  propsWithChildren :: { | attrs }
  propsWithChildren = Record.merge { children } props

