module Contrib.Data.String where

import Prelude

import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Profunctor.Strong ((&&&))
import Data.String (Pattern, stripPrefix)
import Data.String as String

tryStripPrefix :: Pattern -> String -> String
tryStripPrefix pattern str = fromMaybe str (stripPrefix pattern str)

isPrefixOf :: Pattern -> String -> Boolean
isPrefixOf prefixPattern = isJust <<< String.stripPrefix prefixPattern

decodeEnumWith :: forall a. Show a => BoundedEnum a => (String -> String) -> String -> Maybe a
decodeEnumWith adaptConstructorName = do
  let
    -- Let's precompute this `Map`
    values = Map.fromFoldable <<< map (adaptConstructorName <<< show &&& identity) $ (upFromIncluding bottom :: Array a)
  \v -> do
    flip Map.lookup values v

encodeEnumWith :: forall a. Show a => (String -> String) -> a -> String
encodeEnumWith adaptConstructorName = adaptConstructorName <<< show
