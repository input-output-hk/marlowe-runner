module Contrib.Data.Argonaut where

import Prelude

import Contrib.Data.String as S
import Data.Argonaut (Json, JsonDecodeError(..), caseJsonString, fromString, stringify)
import Data.Either (Either(..), note)
import Data.Enum (class BoundedEnum)
import Data.Maybe (Maybe)

type JsonParserResult a = Either JsonDecodeError a
type JsonParser a = Json -> JsonParserResult a

decodeFromString :: forall a. (String -> Maybe a) -> JsonParser a
decodeFromString decode json = do
  let
    decode' str = do
      let
        err = TypeMismatch $ "Unexpected constructor name:" <> str
      note err $ decode str
  caseJsonString
    (Left $ TypeMismatch $ "Unexpected json value: " <> stringify json)
    decode'
    json

decodeJsonEnumWith :: forall a. Show a => BoundedEnum a => (String -> String) -> JsonParser a
decodeJsonEnumWith adaptConstructorName = do
  decodeFromString (S.decodeEnumWith adaptConstructorName)

encodeJsonEnumWith :: forall a. Show a => (String -> String) -> a -> Json
encodeJsonEnumWith adaptConstructorName = fromString <<< adaptConstructorName <<< show

