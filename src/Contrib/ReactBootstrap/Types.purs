module Contrib.ReactBootstrap.Types where

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM.Simplified.ToJSX (class ToJSX, toJSX)
import ReactBootstrap.Types (SelectCallback)
import React.HTMLAttributes (HTMLAttributes)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement)

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


-- export type AlignDirection = 'start' | 'end';
-- export type ResponsiveAlignProp = {
--     sm: AlignDirection;
-- } | {
--     md: AlignDirection;
-- } | {
--     lg: AlignDirection;
-- } | {
--     xl: AlignDirection;
-- } | {
--     xxl: AlignDirection;
-- } | Record<string, AlignDirection>;
-- export type AlignType = AlignDirection | ResponsiveAlignProp;

foreign import data AlignDirection :: Type

alignDirection
  :: { start :: AlignDirection
     , end :: AlignDirection
     }
alignDirection =
  { "start": unsafeCoerce "start"
  , "end": unsafeCoerce "end"
  }

foreign import data ResponsiveAlignProp :: Type

responsiveAlignProp
  :: { sm :: AlignDirection
     , md :: AlignDirection
     , lg :: AlignDirection
     , xl :: AlignDirection
     , xxl :: AlignDirection
     }
responsiveAlignProp =
  { "sm": unsafeCoerce "sm"
  , "md": unsafeCoerce "md"
  , "lg": unsafeCoerce "lg"
  , "xl": unsafeCoerce "xl"
  , "xxl": unsafeCoerce "xxl"
  }

-- Let's flatten the above two into a single type.
foreign import data AlignType :: Type

alignType
  :: { start :: AlignDirection
     , end :: AlignDirection
     , sm :: AlignDirection
     , md :: AlignDirection
     , lg :: AlignDirection
     , xl :: AlignDirection
     , xxl :: AlignDirection
     }
alignType =
  { "start": unsafeCoerce "start"
  , "end": unsafeCoerce "end"
  , "sm": unsafeCoerce "sm"
  , "md": unsafeCoerce "md"
  , "lg": unsafeCoerce "lg"
  , "xl": unsafeCoerce "xl"
  , "xxl": unsafeCoerce "xxl"
  }
