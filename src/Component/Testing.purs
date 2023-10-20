module Component.Testing where

import Foreign.Object (Object)
import Foreign.Object as Object
import Type.Row.Homogeneous as Row

mkDataTestAttrs :: forall r. Row.Homogeneous r String => String -> { | r } -> Object String
mkDataTestAttrs id r = do
  let
    obj = Object.fromHomogeneous r
  Object.insert "testId" id obj
