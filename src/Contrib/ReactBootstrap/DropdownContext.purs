module Contrib.ReactBootstrap.DropdownContext where

import Contrib.ReactBootstrap.Types (AlignType)
import Unsafe.Coerce (unsafeCoerce)

-- import * as React from 'react';
-- import { AlignType } from './types';
-- export type DropDirection = 'up' | 'up-centered' | 'start' | 'end' | 'down' | 'down-centered';
-- export type DropdownContextValue = {
--     align?: AlignType;
--     drop?: DropDirection;
--     isRTL?: boolean;
-- };
-- declare const DropdownContext: React.Context<DropdownContextValue>;
-- export default DropdownContext;
-- export type DropDirection = 'up' | 'up-centered' | 'start' | 'end' | 'down' | 'down-centered';

foreign import data DropDirection :: Type

dropDirection
  :: { up :: DropDirection
     , left :: DropDirection
     , right :: DropDirection
     , down :: DropDirection
     }
dropDirection =
  { "up": unsafeCoerce "up"
  , "left": unsafeCoerce "left"
  , "right": unsafeCoerce "right"
  , "down": unsafeCoerce "down"
  }

type DropdowContextValue =
  { align :: AlignType
  , drop :: DropDirection
  , isRTL :: Boolean
  }
