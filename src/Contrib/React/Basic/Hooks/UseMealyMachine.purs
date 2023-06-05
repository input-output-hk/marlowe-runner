module Contrib.React.Basic.Hooks.UseMealyMachine where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import React.Basic.Hooks (Hook, bind, coerceHook, discard, readRef) as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import Type.Function (type (#))
import Utils.React.Basic.Hooks (UseStateRef, UseVersionedState, useStateRef, useVersionedState')

type MealyMachineStep state action output = state -> action -> (state /\ output)

type MealyMachineSpec state action output =
  { initialState :: state
  -- | Driver can trigger automatic transitions.
  , driver :: state -> Maybe (Aff action)
  , step :: MealyMachineStep state action output
  }

newtype UseMealyMachine :: Type -> Type -> Type -> Type -> Type
newtype UseMealyMachine state action output hooks = UseMealyMachine
  ( hooks
      # UseVersionedState { state :: state, output :: Maybe output }
      # UseStateRef Int state
      # UseVersionedState (Maybe (Aff action))
      # UseStateRef Int (Maybe (Aff action))
      # UseAff Int Unit
  )

derive instance Newtype (UseMealyMachine state action output hooks) _

-- Much more simpler API than `useHalo` which should be probably preferable.
useMealyMachine
  :: forall output state action
   . MealyMachineSpec state action output
  -> React.Hook
      (UseMealyMachine state action output) (state /\ Maybe output /\ (action -> Effect Unit))
useMealyMachine { driver, initialState, step } = React.coerceHook React.do
  { state: { state, output }, version } /\ setState <- useVersionedState' { state: initialState, output: Nothing }
  stateRef <- useStateRef version state

  { state: possibleAffAction, version: requestTrigger } /\ setPossibleAffAction <- useVersionedState' Nothing
  possibleAffActionRef <- useStateRef requestTrigger possibleAffAction

  let
    handleAction action = do
      currState <- React.readRef stateRef
      let
        state' /\ output' = step currState action
        possibleNextAffAction' = driver state'
      setState { state: state', output: Just output' }
      setPossibleAffAction possibleNextAffAction'

  void $ useAff requestTrigger do
    currPossibleAffAction <- liftEffect $ React.readRef possibleAffActionRef
    case currPossibleAffAction of
      Nothing -> pure unit
      Just request -> do
        delay (Milliseconds 2000.0)
        action <- request
        liftEffect $ handleAction action
  pure (state /\ output /\ handleAction)

