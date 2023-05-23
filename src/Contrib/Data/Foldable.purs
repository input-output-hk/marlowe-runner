module Contrib.Data.Foldable where

import Prelude

import Data.Foldable (class Foldable, foldMap)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)

foldMapFlipped :: forall a b t. Foldable t => Monoid b => t a -> (a -> b) -> b
foldMapFlipped = flip foldMap

foldMapWithIndexFlipped :: forall a b f i. FoldableWithIndex i f => Monoid b => f a -> (i -> a -> b) -> b
foldMapWithIndexFlipped = flip foldMapWithIndex

