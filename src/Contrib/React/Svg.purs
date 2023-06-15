module Contrib.React.Svg where

import Data.Array.NonEmpty (elem)
import Data.Newtype (class Newtype, un)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_img, p)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as DOM.SVG
import Record as Record
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype SvgUrl = SvgUrl String

derive instance Newtype SvgUrl _

svgImg
  :: forall attrs attrs' attrs_' attrs_
   . Row.Cons "src" SvgUrl attrs_ attrs
  => Row.Cons "src" String attrs_ attrs'
  => Row.Union attrs' attrs_' Props_img
  => { | attrs }
  -> JSX
svgImg props = do
  let
    props' :: { | attrs' }
    props' = unsafeCoerce props -- Record.modify (Proxy :: Proxy "src") (un SvgUrl) props
  DOM.img props'

foreign import _SvgProgress :: ReactComponent { progress :: Int, svg :: JSX }

svgProgress :: { progress :: Int, svg :: JSX } -> JSX
svgProgress = element _SvgProgress

foreign import _LoadingSpinnerLogo :: ReactComponent {}

loadingSpinnerLogo :: {} -> JSX
loadingSpinnerLogo = element _LoadingSpinnerLogo
