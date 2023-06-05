module Contrib.React.Basic.Hooks.UseMooreMachine where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
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
      , reset :: Maybe (MooreMachineSpec state action output) -> Effect Unit
      }
useMooreMachine initialSpec = React.coerceHook React.do
  { state: spec@{ driver, output, step }, version: specVersion } /\ setSpec <- useVersionedState' initialSpec
  specRef <- useStateRef specVersion spec

  { state, version: stateVersion } /\ setState <- useVersionedState' initialSpec.initialState
  stateRef <- useStateRef stateVersion state

  { state: possibleRequest, version: requestTrigger } /\ setPossibleRequest <- useVersionedState' Nothing
  possibleRequestRef <- useStateRef requestTrigger possibleRequest

  let
    applyAction action = do
      currState <- React.readRef stateRef
      let
        state' = step currState action
        possibleNextRequest' = driver state'
      setState state'
      setPossibleRequest possibleNextRequest'

  void $ useAff requestTrigger do
    currPossibleRequest <- liftEffect $ React.readRef possibleRequestRef
    case currPossibleRequest of
      Nothing -> pure unit
      Just request -> do
        action <- request
        liftEffect $ applyAction action
  pure
    { state
    , output: output state
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
    }

