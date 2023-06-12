module Contrib.JsonBigInt where

import Prelude

import Contrib.Data.Argonaut (JsonString, Reviver)
import Data.Argonaut (Json, fromString)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- A Json which contains possibly `BigInt` values.
foreign import data BigIntJson :: Type

fromJson :: Json -> BigIntJson
fromJson = unsafeCoerce

unsafeToJson :: BigIntJson -> Json
unsafeToJson = unsafeCoerce

toBigInt :: BigIntJson -> Maybe BigInt
toBigInt json = if isBigInt json
  then Just <<< unsafeCoerce $ json
  else Nothing

type ParseResultHandlers a = { failure :: String -> a, success :: BigIntJson -> a}

foreign import patchersImpl ::
  { patchStringify :: Effect Unit
  , patchParse :: Effect Unit
  , parseImpl :: forall a. Fn3 (ParseResultHandlers a) Reviver JsonString a
  }

patchers ::
  { patchStringify :: Effect Unit
  , patchParse :: Effect Unit
  , parse :: Reviver -> JsonString -> Either String BigIntJson
  }
patchers = do
  let
    eitherHandlers = { failure: Left, success: Right }
    parse = runFn3 patchersImpl.parseImpl eitherHandlers
  { parse
  , patchStringify: patchersImpl.patchStringify
  , patchParse: patchersImpl.patchParse
  }

foreign import isBigInt :: forall a. a -> Boolean

replacer :: String -> BigIntJson -> BigIntJson
replacer _ value = case toBigInt value of
  Just bigInt -> fromJson <<< fromString <<< show $ bigInt
  Nothing -> value
