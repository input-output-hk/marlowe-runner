module Contrib.JsonBigInt where

import Prelude

import Data.Argonaut (Json)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- A Json which contains possibly `BigInt` values
foreign import data BigIntJson :: Type

fromJson :: Json -> BigIntJson
fromJson = unsafeCoerce


-- -- toJson :: BigIntJson -> Json
-- -- toJson = 
-- 
-- foreign import stringify :: BigIntJson -> String
-- 
-- foreign import parse :: String -> BigIntJson

foreign import patchers :: { patchStringify :: Effect Unit, patchParse :: Effect Unit }

-- foreign import isBigInt :: forall a. a -> Boolean
-- 
-- replacer :: String -> BigIntJson -> Json
-- replacer key value
--   | isBigInt value = stringify value
--   | otherwise = value
