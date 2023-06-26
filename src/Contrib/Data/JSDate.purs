module Contrib.Data.JSDate where

import Data.JSDate (JSDate)

-- TODO: Add support for options:

foreign import toLocaleDateString :: JSDate -> String

foreign import toLocaleTimeString :: JSDate -> String

