module Contrib.ErrorToJson where

import Data.Argonaut (Json)
import Effect.Exception (Error)

-- Bindings to: https://github.com/tjmehta/error-to-json

foreign import errorToJson :: Error -> Json
