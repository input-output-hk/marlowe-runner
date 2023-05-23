module HexString where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe (unsafeRegex)

newtype Hex = Hex String

derive newtype instance Eq Hex
derive newtype instance EncodeJson Hex
derive newtype instance DecodeJson Hex

hexToString :: Hex -> String
hexToString (Hex str) = str

hex :: String -> Maybe Hex
hex = do
  let
    lowerCaseHexPattern = unsafeRegex "^[0-9a-f]+$" mempty
    anyHexPattern = unsafeRegex "^[0-9a-fA-F]+$" mempty

  case _ of
    str | Regex.test lowerCaseHexPattern str -> Just $ Hex str
    str | Regex.test anyHexPattern str -> Just $ Hex $ toLower str
    _ -> Nothing

foreign import decode :: Hex -> Uint8Array

foreign import encode :: Uint8Array -> Hex

