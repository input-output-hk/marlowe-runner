module Contrib.ReactBootstrap.DropdownButton where

import Contrib.ReactBootstrap.Dropdown (DropdownProps)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- import * as React from 'react';
-- import { DropdownProps } from './Dropdown';
-- import { PropsFromToggle } from './DropdownToggle';
-- import { DropdownMenuVariant } from './DropdownMenu';
-- import { BsPrefixProps, BsPrefixRefForwardingComponent } from './helpers';
-- export interface DropdownButtonProps extends Omit<DropdownProps, 'title'>, PropsFromToggle, BsPrefixProps {
--     title: React.ReactNode;
--     menuRole?: string;
--     renderMenuOnMount?: boolean;
--     rootCloseEvent?: 'click' | 'mousedown';
--     menuVariant?: DropdownMenuVariant;
--     flip?: boolean;
-- }


-- Some not implemented placeholders

type PropsFromToggle :: forall k. k -> k

type PropsFromToggle r = ( | r)

foreign import data DropdownMenuVariant :: Type

foreign import data RootCloseEvent :: Type

rootCloseEvent ::
  { click :: RootCloseEvent
  , mousedown :: RootCloseEvent
  }
rootCloseEvent =
  { click: unsafeCoerce "click"
  , mousedown: unsafeCoerce "mousedown"
  }

type DropdownButtonProps r =
  DropdownProps +
    PropsFromToggle +
    ( title :: JSX -- ReactNode
    , menuRole :: Opt String
    , renderMenuOnMount :: Opt Boolean
    , rootCloseEvent :: Opt RootCloseEvent
    , menuVariant :: Opt DropdownMenuVariant
    , flip :: Opt Boolean
    | r
    )

foreign import _DropdownButton :: ReactComponent { | DropdownButtonProps () }

_internalDropdownButton
  :: forall props
   . NoProblem.Coerce { | props } { | DropdownButtonProps () }
  => { | props } -> JSX
_internalDropdownButton props = do
  let
    props' = NoProblem.coerce props
  element _DropdownButton props'

-- similar to form
dropdownButton
  :: forall jsx props
   . ToJSX jsx
  => Row.Lacks "children" props
  => Row.Cons "children" (Array JSX) props (children :: Array JSX | props)
  => NoProblem.Coerce { children :: Array JSX | props } { | DropdownButtonProps () }
  => { | props }
  -> jsx
  -> JSX
dropdownButton props children = do
  let
    props' :: { children :: Array JSX | props }
    props' = Record.insert (Proxy :: Proxy "children") (toJSX children) props
  _internalDropdownButton props'


