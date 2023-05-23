module Contrib.Data.FunctorWithIndex where

import Prelude

import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)

mapWithIndexFlipped :: forall a b f i. FunctorWithIndex i f => f a -> (i -> a -> b) -> f b
mapWithIndexFlipped = flip mapWithIndex
