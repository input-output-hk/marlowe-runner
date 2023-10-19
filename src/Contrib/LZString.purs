module Contrib.LZString (decompressFromURI) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable

foreign import decompressFromURIImpl :: String -> Nullable String

decompressFromURI :: String -> Maybe String
decompressFromURI = Nullable.toMaybe <<< decompressFromURIImpl
