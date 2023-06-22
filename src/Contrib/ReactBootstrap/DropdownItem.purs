module Contrib.ReactBootstrap.DropdownItem where

import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as NoProblem
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import React.HTMLAttributes (HTMLAttributes')
import ReactBootstrap.Types (EventKey)
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

-- This comes from '@restart/ui/DropdownItem';
--
-- import * as React from 'react';
-- import { EventKey, DynamicRefForwardingComponent } from './types';
-- import Button from './Button';
-- export interface DropdownItemProps extends React.HTMLAttributes<HTMLElement> {
--     /**
--      * Element used to render the component.
--      */
--     as?: React.ElementType;
--     /**
--      * Highlight the menu item as active.
--      */
--     active?: boolean;
--     /**
--      * Disable the menu item, making it unselectable.
--      */
--     disabled?: boolean;
--     /**
--      * Value passed to the `onSelect` handler, useful for identifying the selected menu item.
--      */
--     eventKey?: EventKey;
--     /**
--      * HTML `href` attribute corresponding to `a.href`.
--      */
--     href?: string;
-- }

type DropdownItemProps r =
  HTMLAttributes' +
    ( -- as :: React.ElementType
      active :: Opt Boolean
    , disabled :: Opt Boolean
    , eventKey :: Opt EventKey
    , href :: Opt String
    | r
    )

-- In the original react-bootstrap the Dropdown.Item is a bit more than that.
foreign import _DropdownItem :: ReactComponent { | DropdownItemProps () }

_internalDropdownItem
  :: forall props
   . NoProblem.Coerce { | props } { | DropdownItemProps () }
  => { | props } -> JSX
_internalDropdownItem props = do
  let
    props' = NoProblem.coerce props
  element _DropdownItem props'

dropdownItem
  :: forall jsx props
   . ToJSX jsx
  => Row.Lacks "children" props
  => Row.Cons "children" (Array JSX) props (children :: Array JSX | props)
  => NoProblem.Coerce { children :: Array JSX | props } { | DropdownItemProps () }
  => { | props }
  -> jsx
  -> JSX
dropdownItem props children = do
  let
    props' :: { children :: Array JSX | props }
    props' = Record.insert (Proxy :: Proxy "children") (toJSX children) props
  _internalDropdownItem props'
