module Contrib.Data.Profunctor.Choice where

import Prelude

import Data.Either (Either, either, note)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right)

opt :: forall a b p. Choice p => Profunctor p => p a b -> p (Maybe a) (Maybe b)
opt p = dimap (note unit) (either (const Nothing) Just) (right p)

req :: forall a b err p. Choice p => Profunctor p => err -> p a b -> p (Maybe a) (Either err b)
req err p = dimap (note err) identity (right p)

