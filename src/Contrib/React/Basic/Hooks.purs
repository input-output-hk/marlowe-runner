module Utils.React.Basic.Hooks where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..), Seconds, fromDuration)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Ref as Ref
import Effect.Timer (clearInterval, clearTimeout, setInterval, setTimeout)
import Halogen.Subscription (Emitter, subscribe, unsubscribe) as HS
import React.Basic.Hooks (Hook, Ref, UseEffect, UseRef, UseState, useEffect, useRef, useState, writeRef)
import React.Basic.Hooks (Render) as RB.Hooks
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)

type HookApply hooks (newHook :: Type -> Type) = newHook hooks

-- | Applies a new hook to a hook chain, with the innermost hook as the left argument.
-- | This allows hook chains to be written in reverse order, aligning them with the
-- | order they appear when actually used in do-notation.
-- | ```purescript
-- | type UseCustomHook hooks = UseEffect String (UseState Int hooks)
-- | type UseCustomHook' = UseState Int & UseEffect String
-- | ```
infixl 0 type HookApply as &

-- | Move to components?
type UseDebounce a hooks =
  UseState a hooks
    & UseEffect (a /\ Seconds)

useDebounce :: forall a hooks. Eq a => a -> Seconds -> RB.Hooks.Render hooks (UseDebounce a hooks) a
useDebounce value delay = React.do
  let
    delay' = do
      let
        Milliseconds d = fromDuration delay
      Int.floor d
  debouncedValue /\ setDebouncedValue <- useState value

  useEffect (value /\ delay) do
    i <- setTimeout delay' do
      setDebouncedValue (const value)
    pure $ clearTimeout i
  pure debouncedValue

-- | Use this when you want to handle values and state yourself
newtype UseEmitter hooks = UseEmitter (UseEffect Unit hooks)

derive instance Newtype (UseEmitter hooks) _

useEmitter :: forall a. HS.Emitter a -> (a -> Effect Unit) -> Hook UseEmitter Unit
useEmitter emitter handler =
  React.coerceHook React.do
    React.useEffectOnce $ do
      subscription <- HS.subscribe emitter handler
      pure $ HS.unsubscribe subscription

-- | Use this when you want to have access to the last value emitted
newtype UseEmitter' a hooks = UseEmitter' (UseEffect Unit (UseState a hooks))

derive instance Newtype (UseEmitter' a hooks) _

useEmitter' :: forall a. a -> HS.Emitter a -> Hook (UseEmitter' a) a
useEmitter' default emitter =
  React.coerceHook React.do
    value /\ setValue <- React.useState' default
    React.useEffectOnce $ do
      subscription <- HS.subscribe emitter setValue
      pure $ HS.unsubscribe subscription
    pure value

type UseFirstRender hooks =
  UseRef Boolean hooks
    & UseEffect Unit

-- | The hooks doesn't trigger rerender but is wrapped in `Ref`
-- | and can be used in a `useEffect`.
useFirstRender :: Hook UseFirstRender (Ref Boolean)
useFirstRender = React.do
  firstRender <- React.useRef true
  React.useEffectOnce do
    React.writeRef firstRender false
    pure $ pure unit
  pure firstRender

-- | To avoid "closure capture" in `useEffect` and `useLayoutEffect`
-- | we need to use `useRef` to store the current value.
-- | This hook is a shortcut for that.
newtype UseStateRef v st hooks = UseStateRef (UseEffect v (UseRef st hooks))

derive instance Newtype (UseStateRef v st hooks) _

useStateRef :: forall st v. Eq v => v -> st -> Hook (UseStateRef v st) (Ref st)
useStateRef version state =
  React.coerceHook $ React.do
    stateRef <- useRef state
    useEffect version do
      writeRef stateRef state
      pure $ pure unit
    pure stateRef

useStateRef' :: forall st. Eq st => st -> Hook (UseStateRef st st) (Ref st)
useStateRef' st = useStateRef st st

-- Run an action on regular basis. Use the cleanup action when unmounting or on deps change.
useSetInterval
  :: forall deps
   . Eq deps
  => deps
  -> Milliseconds
  -> Effect (Effect Unit)
  -> Hook (UseEffect deps) Unit
useSetInterval deps (Milliseconds milliseconds) action = React.do
  useEffect deps do
    cleanupRef <- Ref.new Nothing
    intervalId <- setInterval (Int.floor milliseconds) do
      cleanup <- action
      Ref.write (Just cleanup) cleanupRef

    pure $ do
      Ref.read cleanupRef >>= fromMaybe (pure unit)
      clearInterval intervalId

useLoopAff
  :: forall deps
   . Eq deps
  => deps
  -> Milliseconds
  -> Aff Unit
  -> Hook (UseAff deps Unit) Unit
useLoopAff deps interval action = React.do
  void $ useAff deps $ forever $ do
    action
    delay interval

