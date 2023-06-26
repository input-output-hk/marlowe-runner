module React.Basic.Hooks.UseStatelessFormSpec where

-- | This hook is render agnostic and you construct rendering
-- | function during `Form` type composition.
-- | You can use any monoidal stracture for the resulting
-- | rendering - it can be `JSX`, `Map String JSX` or even
-- | `Foreign.Object Json` so the result of the validation
-- | can be sent over the wire.

import Prelude

import Contrib.Polyform.FormSpecs.StatelessFormSpec (FieldInitials, StatelessFormSpec(..), FieldInitialsRow)
import Data.Either (Either(..))
import Data.Foldable (foldMap, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as Query
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Seconds)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Validator (runValidator)
import React.Basic.Events (EventHandler, SyntheticEvent, handler_)
import React.Basic.Hooks (type (&), type (/\), Hook, UseEffect, UseMemo, UseState, useEffect, useMemo, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseDebounce (useDebounce)
import Safe.Coerce (coerce)
import Type.Row (type (+))

type Props doc err o =
  { onSubmit ::
      { payload :: Query
      , result :: Maybe ((V (UrlEncoded.Errors err) o) /\ Query)
      }
      -> Effect Unit
  , spec :: StatelessFormSpec Effect doc err Query o
  , validationDebounce :: Seconds
  }

newtype UseStatlessFormSpec err o hooks = UseStatlessFormSpec
  ( UseState (Set FieldId) hooks
      & UseState (Maybe ((V (UrleEncoded.Errors err) o) /\ Query))
      & UseMemo (Array FieldInitials) Query
      & UseState Query
      & UseState Query
      & UseEffect (Query /\ Seconds)
      & UseEffect Query
  )

derive instance Newtype (UseStatlessFormSpec o err hooks) _

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

useStatelessFormSpec :: forall doc err o. Props doc err o -> Hook (UseStatlessFormSpec err o) (Result err o)
useStatelessFormSpec ({ spec: StatelessFormSpec { fields, validator }, onSubmit, validationDebounce }) = React.coerceHook React.do
  touched /\ updateTouched <- useState $ flip foldMap fields \{ name, touched } ->
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

