module Contrib.ReactBootstrap.Dropdown where

import Prelude

import Contrib.ReactBootstrap.DropdownContext (DropDirection)
import Contrib.ReactBootstrap.Types (AlignType)
import Data.Undefined.NoProblem (Opt)
import Effect.Uncurried (EffectFn2)
import React.Basic (JSX)
import ReactBootstrap.Types (Placement)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- These is originaly `DropdownProps` from '@restart/ui/Dropdown';
-- export interface BaseDropdownProps {
--     /**
--      * The PopperJS placement for positioning the Dropdown menu in relation to
--      * its Toggle.
--      *
--      * @default 'bottom-start'
--      */
--     placement?: Placement;
--     /**
--      * Sets the initial visibility of the Dropdown.
--      */
--     defaultShow?: boolean;
--     /**
--      * Whether or not the Dropdown is visible.
--      *
--      * @controllable onToggle
--      */
--     show?: boolean;
--     /**
--      * A callback fired when a DropdownItem has been selected.
--      */
--     onSelect?: SelectCallback;
--     /**
--      * A callback fired when the Dropdown wishes to change visibility. Called with
--      * the requested `show` value, the DOM event, and the source that fired it:
--      * `'click'`,`'keydown'`,`'rootClose'`, or `'select'`.
--      *
--      * ```ts static
--      * function(
--      *   nextShow: boolean,
--      *   meta: ToggleMetadata,
--      * ): void
--      * ```
--      *
--      * @controllable show
--      */
--     onToggle?: (nextShow: boolean, meta: ToggleMetadata) => void;
--     /**
--      * A css selector string that will return __focusable__ menu items.
--      * Selectors should be relative to the menu component:
--      * e.g. ` > li:not('.disabled')`
--      */
--     itemSelector?: string;
--     /**
--      * Controls the focus behavior for when the Dropdown is opened. Set to
--      * `true` to always focus the first menu item, `keyboard` to focus only when
--      * navigating via the keyboard, or `false` to disable completely
--      *
--      * The Default behavior is `false` **unless** the Menu has a `role="menu"`
--      * where it will default to `keyboard` to match the recommended [ARIA Authoring
--      * practices](https://www.w3.org/TR/wai-aria-practices-1.1/#menubutton).
--      */
--     focusFirstItemOnShow?: boolean | 'keyboard';
--     /**
--      * A render prop that returns the root dropdown element. The `props`
--      * argument should spread through to an element containing _both_ the
--      * menu and toggle in order to handle keyboard events for focus management.
--      *
--      * @type {Function ({
--      *   props: {
--      *     onKeyDown: (SyntheticEvent) => void,
--      *   },
--      * }) => React.Element}
--      */
--     children: React.ReactNode;
-- }

foreign import data FocusFirstItemOnShow :: Type

focusFirstItemOnShow
  :: { keyboard :: FocusFirstItemOnShow
     , false :: FocusFirstItemOnShow
     , true :: FocusFirstItemOnShow
     }
focusFirstItemOnShow =
  { "keyboard": unsafeCoerce "keyboard"
  , "false": unsafeCoerce "false"
  , "true": unsafeCoerce "true"
  }

foreign import data ToggleMetadata :: Type

type BaseDropdownProps r =
  ( placement :: Opt Placement
  , defaultShow :: Opt Boolean
  -- , onSelect :: SelectCallback
  , onToggle :: Opt (EffectFn2 Boolean ToggleMetadata Unit)
  , itemSelector :: Opt String
  , focusFirstItemOnShow :: Opt FocusFirstItemOnShow
  , children :: Array JSX
  | r
  )

-- import * as React from 'react';
-- import { DropdownProps as BaseDropdownProps } from '@restart/ui/Dropdown';
-- import { DropDirection } from './DropdownContext';
-- import { BsPrefixProps, BsPrefixRefForwardingComponent } from './helpers';
-- import { AlignType } from './types';
-- export interface DropdownProps extends BaseDropdownProps, BsPrefixProps, Omit<React.HTMLAttributes<HTMLElement>, 'onSelect' | 'children'> {
--     drop?: DropDirection;
--     align?: AlignType;
--     focusFirstItemOnShow?: boolean | 'keyboard';
--     navbar?: boolean;
--     autoClose?: boolean | 'outside' | 'inside';
-- }

foreign import data AutoClose :: Type

autoClose
  :: { outside :: AutoClose
     , inside :: AutoClose
     , false :: AutoClose
     , true :: AutoClose
     }
autoClose =
  { "outside": unsafeCoerce "outside"
  , "inside": unsafeCoerce "inside"
  , "false": unsafeCoerce "false"
  , "true": unsafeCoerce "true"
  }

type DropdownProps r =
  BaseDropdownProps +
    ( className :: Opt String
    , drop :: Opt DropDirection
    , align :: Opt AlignType
    , focusFirstItemOnShow :: Opt FocusFirstItemOnShow
    , navbar :: Opt Boolean
    , autoClose :: Opt AutoClose
    | r
    )
