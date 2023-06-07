module Contrib.React.Basic.Hooks.UseMooreMachine where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React.Basic.Hooks (Hook, bind, coerceHook, discard, readRef) as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import Type.Function (type (#))
import Utils.React.Basic.Hooks (UseStateRef, UseVersionedState, useStateRef, useVersionedState')

type MooreMachineStep state action = state -> action -> state

type MooreMachineDriver state action = state -> Maybe (Aff action)

type MooreMachineSpec state action output =
  { initialState :: state
  -- | Driver can trigger automatic actions.
  , driver :: MooreMachineDriver state action
  -- , onStateTransition :: state -> state -> Effect Unit
  , output :: state -> output
  , step :: MooreMachineStep state action
  }

newtype UseMooreMachine :: Type -> Type -> Type -> Type -> Type
newtype UseMooreMachine state action output hooks = UseMooreMachine
  ( hooks
      # UseVersionedState (MooreMachineSpec state action output)
      # UseStateRef Int (MooreMachineSpec state action output)
      # UseVersionedState state
      # UseStateRef Int state
      # UseVersionedState (Maybe (Aff action))
      # UseStateRef Int (Maybe (Aff action))
      # UseAff Int Unit
  )

derive instance Newtype (UseMooreMachine state action output hooks) _

-- Much more simpler API than `useHalo` which should be probably preferable.
useMooreMachine
  :: forall output state action
   . MooreMachineSpec state action output
  -> React.Hook
      (UseMooreMachine state action output)
      { state :: state
      , output :: output
      , applyAction :: action -> Effect Unit
      -- If just after the reset in the handler you have to use `applyAction` then
      -- you can use the returned value to do it.
      , reset :: Maybe (MooreMachineSpec state action output) -> Effect (action -> Effect Unit)
      }
useMooreMachine initialSpec = React.coerceHook React.do
  { state: spec, version: specVersion } /\ setSpec <- useVersionedState' initialSpec
  specRef <- useStateRef specVersion spec

  { state, version: stateVersion } /\ setState <- useVersionedState' initialSpec.initialState
  stateRef <- useStateRef stateVersion state

  { state: possibleRequest, version: requestTrigger } /\ setPossibleRequest <- useVersionedState' Nothing
  possibleRequestRef <- useStateRef requestTrigger possibleRequest

  let
    applyActionFn action currState currSpec = do
      let
        state' = currSpec.step currState action
        possibleNextRequest' = currSpec.driver state'
      setState state'
      setPossibleRequest possibleNextRequest'

    applyAction action = do
      currState <- React.readRef stateRef
      currSpec <- React.readRef specRef
      applyActionFn action currState currSpec

  void $ useAff requestTrigger do
    currPossibleRequest <- liftEffect $ React.readRef possibleRequestRef
    case currPossibleRequest of
      Nothing -> pure unit
      Just request -> do
        action <- request
        liftEffect $ applyAction action
  pure
    { state
    , output: spec.output state
    , applyAction
    , reset: \possibleNewSpec -> do
        currSpec <- React.readRef specRef
        let
          spec' = fromMaybe
            currSpec
            possibleNewSpec

        setSpec spec'
        setState spec'.initialState
        setPossibleRequest Nothing
        pure \action -> applyActionFn action spec'.initialState spec'
    }

