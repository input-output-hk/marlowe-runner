module Contrib.React.Basic.Hooks.UseForm where

-- | This hook is render agnostic and you construct rendering
-- | function during `Form` type composition.
-- | You can use any monoidal stracture for the resulting
-- | rendering - it can be `JSX`, `Map String JSX` or even
-- | `Foreign.Object Json` so the result of the validation
-- | can be sent over the wire.

import Prelude

import Contrib.Data.Foldable (foldMapFlipped)
import Contrib.Polyform.Batteries.UrlEncoded (fieldForm)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (null)
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
import React.Basic.Hooks (type (&), type (/\), Hook, UseEffect, UseMemo, UseState, useEffect, useMemo, useState, useState', (/\))
import React.Basic.Hooks as React
import Safe.Coerce (coerce)
import Type.Row (type (+))
import Utils.React.Basic.Hooks (useDebounce)

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

type RenderFn err doc = FormState err -> doc

newtype Form m doc err i o = Form
  { fields :: Array FieldInitials
  , validator :: UrlEncoded.Validator m err i o
  , render :: RenderFn err doc
  }

renderForm :: forall doc err i o m. Form m doc err i o -> FormState err -> doc
renderForm (Form { render }) = render

mapRender :: forall doc doc' err i o m. (doc -> doc') -> Form m doc err i o -> Form m doc' err i o
mapRender f (Form { fields, validator, render }) = Form { fields, validator, render: f <<< render }

derive instance Applicative m => Functor (Form m doc err i)

instance (Monad m, Semigroup doc) => Apply (Form m doc err i) where
  apply (Form { fields: fields1, validator: validator1, render: render1 }) (Form { fields: fields2, validator: validator2, render: render2 }) =
    Form
      { fields: fields1 <> fields2
      , validator: apply validator1 validator2
      , render: (<>) <$> render1 <*> render2
      }

instance (Monad m, Monoid doc) => Applicative (Form m doc err i) where
  pure a = Form { fields: [], validator: pure a, render: const mempty }

instance (Monad m, Semigroup doc) => Semigroupoid (Form m doc err) where
  compose (Form { fields: fields1, validator: validator1, render: render1 }) (Form { fields: fields2, validator: validator2, render: render2 }) =
    Form
      { fields: fields1 <> fields2
      , validator: compose validator1 validator2
      , render: (<>) <$> render2 <*> render1
      }

instance (Monad m, Monoid doc) => Category (Form m doc err) where
  identity = Form { fields: mempty, validator: identity, render: mempty }

liftValidator :: forall m doc err i o. Monoid doc => UrlEncoded.Validator m err i o -> Form m doc err i o
liftValidator validator = Form { fields: [], validator, render: const mempty }

hoistForm :: forall doc err m m' i o. Functor m => (m ~> m') -> Form m doc err i o -> Form m' doc err i o
hoistForm f (Form { fields, validator, render }) =
  Form { fields, validator: Validator.hoist f validator, render }

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
  :: forall a doc err m
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Batteries.Validator m err (Maybe String) a
  -> Form m doc err Query a
input name initial render touched validator = Form
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: fieldForm name validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

optInput
  :: forall a doc err m
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Boolean
  -> Validators.SingleValueFieldValidator m err a
  -> Form m doc err Query (Maybe a)
optInput name initial render touched validator = Form
  { fields: [ { name, initial: [ initial ], touched } ]
  , validator: Validators.optional name $ validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

multiSelect
  :: forall a doc err m
   . Monad m
  => FieldId
  -> Array String
  -> err
  -> RenderFn err doc
  -> Boolean
  -> Batteries.Validator m err (NonEmptyArray String) (NonEmptyArray a)
  -> Form m doc err Query (NonEmptyArray a)
multiSelect name initial err render touched validator = Form
  { fields: [ { name, initial, touched } ]
  , validator: Validators.requiredMulti name err $ validator
  , render
  }

type Props doc err o =
  { onSubmit ::
      { payload :: Query
      , result :: Maybe ((V (UrlEncoded.Errors err) o) /\ Query)
      }
      -> Effect Unit
  , spec :: Form Effect doc err Query o
  , validationDebounce :: Seconds
  }

newtype UseForm err o hooks = UseForm
  ( UseState (Set FieldId) hooks
      & UseState (Maybe ((V (UrleEncoded.Errors err) o) /\ Query))
      & UseMemo (Array FieldInitials) Query
      & UseState Query
      & UseState Query
      & UseEffect (Query /\ Seconds)
      & UseEffect Query
  )

derive instance Newtype (UseForm o err hooks) _

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

type FormState err =
  { fields :: FieldsState err
  , errors :: Maybe (UrleEncoded.Errors err /\ Query)
  , query :: Query
  -- , state :: state
  }

type Result err o =
  { formState :: FormState err
  , onSubmit :: EffectFn1 SyntheticEvent Unit
  , result :: Maybe ((V (UrlEncoded.Errors err) o) /\ Query)
  }

useForm :: forall doc err o. Props doc err o -> Hook (UseForm err o) (Result err o)
useForm ({ spec: Form { fields, validator }, onSubmit, validationDebounce }) = React.coerceHook React.do
  touched /\ updateTouched <- useState $ foldMapFlipped fields \{ name, touched } ->
    if touched then Set.singleton name else Set.empty
  validationResult /\ setValidationResult <- useState' Nothing

  initialQuery <- useMemo fields \_ -> do
    let
      query = fields <#> \{ name, initial } -> name /\ initial
    Query.fromFoldable query

  currQuery /\ updateQuery <- useState initialQuery
  debouncedQuery <- useDebounce currQuery validationDebounce

  useEffect debouncedQuery do
    when (not <<< null $ touched) do
      result <- runValidator validator debouncedQuery
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
      }
  pure { formState: formState, onSubmit: onSubmit', result: validationResult }

