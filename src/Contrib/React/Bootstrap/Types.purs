module Contrib.React.Bootstrap.Types where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Color :: Type

color :: { danger :: Color, dark :: Color, info :: Color, light :: Color, muted :: Color, primary :: Color, secondary :: Color, warning :: Color, white :: Color }
color =
  { primary: unsafeCoerce "primary" :: Color
  , secondary: unsafeCoerce "secondary" :: Color
  , danger: unsafeCoerce "danger" :: Color
  , warning: unsafeCoerce "warning" :: Color
  , info: unsafeCoerce "info" :: Color
  , dark: unsafeCoerce "dark" :: Color
  , light: unsafeCoerce "light" :: Color
  , white: unsafeCoerce "white" :: Color
  , muted: unsafeCoerce "muted" :: Color
  }

-- The source of this type is not easy to find and parse:
-- node_modules/@popperjs/core/lib/enums.d.ts
foreign import data Placement :: Type

placement
  :: { auto :: Placement
     , "auto-end" :: Placement
     , "auto-start" :: Placement
     , bottom :: Placement
     , "bottom-end" :: Placement
     , "bottom-start" :: Placement
     , left :: Placement
     , "left-end" :: Placement
     , "left-start" :: Placement
     , right :: Placement
     , "right-end" :: Placement
     , "right-start" :: Placement
     , top :: Placement
     , "top-end" :: Placement
     , "top-start" :: Placement
     }
placement =
  { -- BasePlacement
    top: unsafeCoerce "top" :: Placement
  , bottom: unsafeCoerce "bottom" :: Placement
  , right: unsafeCoerce "right" :: Placement
  , left: unsafeCoerce "left" :: Placement
  , auto: unsafeCoerce "auto" :: Placement
  -- VariationPlacement
  , "top-start": unsafeCoerce "top-start" :: Placement
  , "top-end": unsafeCoerce "top-end" :: Placement
  , "bottom-start": unsafeCoerce "bottom-start" :: Placement
  , "bottom-end": unsafeCoerce "bottom-end" :: Placement
  , "right-start": unsafeCoerce "right-start" :: Placement
  , "right-end": unsafeCoerce "right-end" :: Placement
  , "left-start": unsafeCoerce "left-start" :: Placement
  , "left-end": unsafeCoerce "left-end" :: Placement
  -- AutoPlacement
  , "auto-start": unsafeCoerce "auto-start" :: Placement
  , "auto-end": unsafeCoerce "auto-end" :: Placement
  }

foreign import data Variant :: Type

variant
  :: { danger :: Variant
     , dark :: Variant
     , info :: Variant
     , light :: Variant
     , primary :: Variant
     , secondary :: Variant
     , success :: Variant
     , warning :: Variant
     }
variant =
  { primary: unsafeCoerce "primary" :: Variant
  , secondary: unsafeCoerce "secondary" :: Variant
  , success: unsafeCoerce "success" :: Variant
  , danger: unsafeCoerce "danger" :: Variant
  , warning: unsafeCoerce "warning" :: Variant
  , info: unsafeCoerce "info" :: Variant
  , dark: unsafeCoerce "dark" :: Variant
  , light: unsafeCoerce "light" :: Variant
  }

foreign import data Backdrop :: Type

backdrop :: { static :: Backdrop, true :: Backdrop, false :: Backdrop }
backdrop =
  { static: unsafeCoerce "static" :: Backdrop
  , true: unsafeCoerce true :: Backdrop
  , false: unsafeCoerce false :: Backdrop
  }

