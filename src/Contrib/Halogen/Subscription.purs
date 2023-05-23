module Contrib.Halogen.Subscription where

import Prelude

import Data.Foldable (traverse_)
import Data.Int (floor)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Halogen.Subscription (Emitter, Subscription)
import Unsafe.Coerce (unsafeCoerce)

newtype MinInterval = MinInterval Milliseconds

-- | Orig library doesn't expose `Emitter` constructor. We need it to implement
-- | throttling transformation functions with memory safe manner (we
-- | should not `Subscribe.create` internally because we gonna leak
-- | subscription).
unEmitter :: forall a. Emitter a -> ((a -> Effect Unit) -> Effect Subscription)
unEmitter = unsafeCoerce

_Emitter :: forall a. ((a -> Effect Unit) -> Effect Subscription) -> Emitter a
_Emitter = unsafeCoerce

-- | Throttle events and emit only last one in a given interval.
throttle :: forall a. MinInterval -> Emitter a -> Effect (Emitter a)
throttle (MinInterval minInterval) origEmitter = do
  let
    origEmitterFun = unEmitter origEmitter
  valueRef <- Ref.new Nothing
  timeoutIdRef <- Ref.new Nothing

  pure $ _Emitter \k -> do
    origEmitterFun \a -> do
      Ref.write (Just a) valueRef
      Ref.read timeoutIdRef >>= isNothing >>> flip when do
        timeoutId <- setTimeout (floor <<< un Milliseconds $ minInterval) do
          Ref.write Nothing timeoutIdRef
          Ref.read valueRef >>= traverse_ k
          Ref.write Nothing valueRef
        Ref.write (Just timeoutId) timeoutIdRef

-- | Fold over events with given function during given interval and emit the result.
foldMapThrottle :: forall a b. Monoid b => (a -> b) -> MinInterval -> Emitter a -> Effect (Emitter b)
foldMapThrottle f (MinInterval minInterval) origEmitter = do
  let
    origEmitterFun = unEmitter origEmitter
  valueRef <- Ref.new mempty
  timeoutIdRef <- Ref.new Nothing

  pure $ _Emitter \k -> do
    origEmitterFun \a -> do
      Ref.modify_ (flip append $ f a) valueRef
      Ref.read timeoutIdRef >>= isNothing >>> flip when do
        timeoutId <- setTimeout (floor <<< un Milliseconds $ minInterval) do
          Ref.write Nothing timeoutIdRef
          Ref.read valueRef >>= k
          Ref.write mempty valueRef
        Ref.write (Just timeoutId) timeoutIdRef

bindEffect :: forall a b. (a -> Effect b) -> Emitter a -> Emitter b
bindEffect f origEmitter = _Emitter \k -> do
  let
    origEmitterFun = unEmitter origEmitter
  origEmitterFun $ f >=> k
