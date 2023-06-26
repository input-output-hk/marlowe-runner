module Contrib.React.Basic.Hooks.UseStatefulFormSpec where

-- | This hook is render agnostic and you construct rendering
-- | function during `StatefulForm` type composition.
-- | You can use any monoidal stracture for the resulting
-- | rendering - it can be `JSX`, `Map String JSX` or even
-- | `Foreign.Object Json` so the result of the validation
-- | can be sent over the wire.

import Prelude

import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (foldMap, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as Query
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, un)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Seconds)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Batteries.UrlEncoded.Validators as Validators
import Polyform.Validator (runValidator)
import Polyform.Validator as Validator
import React.Basic.Events (EventHandler, SyntheticEvent, handler_)
import React.Basic.Hooks (type (&), type (/\), Hook, UseEffect, UseMemo, UseRef, UseState, useEffect, useMemo, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseDebounce (useDebounce)
import React.Basic.Hooks.UseForm (Form)
import React.Basic.Hooks.UseForm as UseForm
import Safe.Coerce (coerce)
import Type.Row (type (+))

fieldStatefulForm
  :: forall a m err st
   . Monad m
  => FieldId
  -> Batteries.Validator (StateT st m) err (Maybe String) a
  -> UrlEncoded.Validator (StateT st m) err Query a
fieldStatefulForm name fieldValidator = do
  let
    validator' = fieldValidator <<< Validator.liftFn \query -> do
      value <- Query.lookup name query
      Array.head value
  UrlEncoded.fromValidator name validator'

type FieldInitialsRow r =
  ( name :: FieldId
  , initial :: Array String
  -- Should we treat the field as "touched" from the begining
  | r
  )

type FieldInitials =
  { touched :: Boolean
  | FieldInitialsRow ()
  }

type RenderFn :: Row Type -> Type -> Type -> Type
type RenderFn st err doc = StatefulFormState st err -> doc

newtype StatefulForm m st doc err i o = StatefulForm
  { fields :: Array FieldInitials
  , validator :: UrlEncoded.Validator (StateT st m) err i o
  , render :: RenderFn (state :: st) err doc
  }

renderStatefulForm :: forall doc err i o m st. StatefulForm m st doc err i o -> StatefulFormState (state :: st) err -> doc
renderStatefulForm (StatefulForm { render }) = render

mapRender :: forall doc doc' err i o m st. (doc -> doc') -> StatefulForm m st doc err i o -> StatefulForm m st doc' err i o
mapRender f (StatefulForm { fields, validator, render }) = StatefulForm { fields, validator, render: f <<< render }

liftStatelessForm :: forall doc err i o m st. Monad m => Form m doc err i o -> StatefulForm m st doc err i o
liftStatelessForm (UseForm.Form { fields, validator, render }) = StatefulForm
  { fields
  , validator: Validator.hoist lift validator
  , render: \{ fields: fs, errors, query } -> render { fields: fs, errors, query }
  }

derive instance Monad m => Functor (StatefulForm m st doc err i)

instance (Monad m, Semigroup doc) => Apply (StatefulForm m st doc err i) where
  apply (StatefulForm { fields: fields1, validator: validator1, render: render1 }) (StatefulForm { fields: fields2, validator: validator2, render: render2 }) =
    StatefulForm
      { fields: fields1 <> fields2
      , validator: apply validator1 validator2
      , render: (<>) <$> render1 <*> render2
      }

instance (Monad m, Monoid doc) => Applicative (StatefulForm m st doc err i) where
  pure a = StatefulForm { fields: [], validator: pure a, render: const mempty }

instance (Monad m, Semigroup doc) => Semigroupoid (StatefulForm m st doc err) where
  compose (StatefulForm { fields: fields1, validator: validator1, render: render1 }) (StatefulForm { fields: fields2, validator: validator2, render: render2 }) =
    StatefulForm
      { fields: fields1 <> fields2
      , validator: compose validator1 validator2
      , render: (<>) <$> render2 <*> render1
      }

instance (Monad m, Monoid doc) => Category (StatefulForm m st doc err) where
  identity = StatefulForm { fields: mempty, validator: identity, render: mempty }

liftValidator :: forall m doc err i o st. Monoid doc => UrlEncoded.Validator (StateT st m) err i o -> StatefulForm m st doc err i o
liftValidator validator = StatefulForm { fields: [], validator, render: const mempty }

hoistStatefulForm :: forall doc err m m' i o st. Functor m => (StateT st m ~> StateT st m') -> StatefulForm m st doc err i o -> StatefulForm m' st doc err i o
hoistStatefulForm f (StatefulForm { fields, validator, render }) =
  StatefulForm { fields, validator: Validator.hoist f validator, render }

type InputFieldStateRow err r =
  ( errors :: Maybe (Array err /\ Array String) -- `Maybe` indicates if a field was validated
  , onChange :: String -> Effect Unit
  , touched :: Boolean
  , value :: String
  | FieldInitialsRow
      + r
  )

type InputState err = { | InputFieldStateRow err () }

type RenderInputFn err doc = InputState err -> doc

toInputState :: forall err. FieldState err -> InputState err
toInputState fieldState =
  { errors: fieldState.errors
  , onChange: fieldState.onChange <<< Array.singleton
  , touched: un Disj fieldState.touched
  , value: fromMaybe "" $ Array.head fieldState.value
  , name: fieldState.name
  , initial: fieldState.initial
  }

input
  :: forall a doc err m st
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Batteries.Validator (StateT st m) err (Maybe String) a
  -> StatefulForm m st doc err Query a
input name initial render touched validator = StatefulForm
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: fieldStatefulForm name validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

optInput
  :: forall a doc err m st
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Validators.SingleValueFieldValidator (StateT st m) err a
  -> StatefulForm m st doc err Query (Maybe a)
optInput name initial render touched validator = StatefulForm
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: Validators.optional name $ validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

multiSelect
  :: forall a doc err m st
   . Monad m
  => FieldId
  -> Array String
  -> err
  -> RenderFn (state :: st) err doc
  -> Boolean
  -> Batteries.Validator (StateT st m) err (NonEmptyArray String) (NonEmptyArray a)
  -> StatefulForm m st doc err Query (NonEmptyArray a)
multiSelect name initial err render touched validator = StatefulForm
  { fields: [ { name, initial, touched } ]
  , validator: Validators.requiredMulti name err $ validator
  , render
  }

type Props st doc err o =
  { onSubmit ::
      { payload :: Query
      , result :: Maybe ((V (UrlEncoded.Errors err) o) /\ Query)
      }
      -> Effect Unit
  , spec :: StatefulForm Effect st doc err Query o
  , validationDebounce :: Seconds
  , state :: st
  }

newtype UseStatefulForm st err o hooks = UseStatefulForm
  ( UseState (Set FieldId) hooks
      & UseState (Maybe ((V (UrleEncoded.Errors err) o) /\ Query))
      & UseState st
      & UseRef st
      & UseEffect st
      & UseMemo (Array FieldInitials) Query
      & UseState Query
      & UseState Query
      & UseEffect (Query /\ Seconds)
      & UseEffect Query
  )

derive instance Newtype (UseStatefulForm st o err hooks) _

type FieldStateRow err r =
  ( errors :: Maybe (Array err /\ Array String) -- `Maybe` indicates if a field was validated
  , onChange :: Array String -> Effect Unit
  , touched :: Disj Boolean
  , value :: Array String
  | FieldInitialsRow
      + r
  )

type FieldState err = { | FieldStateRow err () }

type RenderFieldFn err doc = FieldState err -> doc

type FieldsState err = Map FieldId (FieldState err)

type StatefulFormState st err =
  { fields :: FieldsState err
  , errors :: Maybe (UrleEncoded.Errors err /\ Query)
  , query :: Query
  | st
  }

type Result st err o =
  { formState :: StatefulFormState st err
  , onSubmit :: EffectFn1 SyntheticEvent Unit
  , result :: Maybe ((V (UrlEncoded.Errors err) o) /\ Query)
  }

useStatefulForm :: forall doc err o st. Eq st => Props st doc err o -> Hook (UseStatefulForm st err o) (Result (state :: st) err o)
useStatefulForm ({ spec: StatefulForm { fields, validator }, onSubmit, validationDebounce, state }) = React.coerceHook React.do
  touched /\ updateTouched <- useState $ flip foldMap fields \{ name, touched } ->
    if touched then Set.singleton name else Set.empty
  validationResult /\ setValidationResult <- useState' Nothing

  internalStatefulFormState /\ setInternalStatefulFormState <- useState' state
  internalStateRef <- React.useRef state
  useEffect internalStatefulFormState do
    React.writeRef internalStateRef internalStatefulFormState
    pure $ pure unit

  initialQuery <- useMemo fields \_ -> do
    let
      query = fields <#> \{ name, initial } -> name /\ initial
    Query.fromFoldable query

  currQuery /\ updateQuery <- useState initialQuery
  debouncedQuery <- useDebounce currQuery validationDebounce

  useEffect debouncedQuery do
    when (not <<< null $ touched) do
      result /\ state' <- flip runStateT state $ (runValidator  validator) debouncedQuery
      setInternalStatefulFormState state'
      setValidationResult $ Just (result /\ debouncedQuery)
    pure $ pure unit

  let
    updateField :: FieldId -> Array String -> Effect Unit
    updateField name value = do
      updateQuery $ Query.insert name value
      updateTouched (Set.insert name)

    onSubmit' :: EventHandler
    onSubmit' = handler_ do
      onSubmit { payload: currQuery, result: validationResult }

    fieldsState = Map.fromFoldable $ fields <#> \{ name, initial } -> do
      let
        value = fromMaybe [] $ Query.lookup name currQuery
        fieldErrors = do
          V res /\ validationQuery <- validationResult
          case res of
            Left errs -> do
              let
                errs' = Errors.lookup (coerce name) errs
              val' <- Query.lookup name validationQuery
              pure $ errs' /\ val'
            Right _ -> do
              val' <- Query.lookup name validationQuery
              pure $ [] /\ val'
      (name /\ { name, initial, value, errors: fieldErrors, touched: Disj (name `Set.member` touched), onChange: updateField name })
    formState =
      { fields: fieldsState
      , errors: validationResult >>= case _ of
          V (Left errs) /\ query -> Just (errs /\ query)
          V (Right _) /\ _ -> Nothing
      , query: currQuery
      , state
      }
  pure { formState: formState, onSubmit: onSubmit', result: validationResult }

