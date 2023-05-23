module Contrib.Data.Map where

import Prelude

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, un)
import Data.Profunctor.Strong ((&&&))

fromFoldableBy :: forall f k v. Functor f => Foldable f => Ord k => (v -> k) -> f v -> Map k v
fromFoldableBy f = Map.fromFoldable <<< map (f &&& identity)

newtype Old k v = Old (Map k v)

derive instance Newtype (Old k v) _

newtype New k v = New (Map k v)

derive instance Newtype (New k v) _

additions :: forall k v. Ord k => Old k v -> New k v -> Map k v
additions (Old old) (New new) = new `Map.difference` old

deletions :: forall k v. Ord k => Old k v -> New k v -> Map k v
deletions (Old old) (New new) = old `Map.difference` new

updates :: forall k v. Eq v => Ord k => Old k v -> New k v -> Map k { old :: v, new :: v }
updates (Old oldMap) = Map.filter (\{ old, new } -> old /= new) <<< Map.intersectionWith { old: _, new: _ } oldMap <<< un New
