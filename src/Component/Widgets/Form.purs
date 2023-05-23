module Component.Widgets.Form where

import Prelude

import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Either (Either(..))
import Data.Foldable (length, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as Query
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Seconds)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Random (random)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1)
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Types (stringifyValidator)
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Batteries.UrlEncoded.Validators as Validators
import Polyform.Validator (runValidator)
import Polyform.Validator as Validator
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, SyntheticEvent, handler, handler_)
import React.Basic.Hooks (type (&), type (/\), Hook, UseEffect, UseMemo, UseState, component, useEffect, useEffectOnce, useMemo, useState, useState', (/\))
import React.Basic.Hooks as React
import Safe.Coerce (coerce)
import Type.Row (type (+))
import Utils.React.Basic.Hooks (useDebounce)

type RadioFieldChoice = String /\ JSX /\ Boolean

choice :: String -> JSX -> RadioFieldChoice
choice value label = value /\ label /\ false

type SelectFieldChoice = String /\ String /\ Boolean

option :: String -> String -> SelectFieldChoice
option value label = value /\ label /\ false

data SingleChoiceField
  = RadioButtonField (ArrayAL 1 RadioFieldChoice) -- use `solo` / `solo'` to create
  | SelectField (ArrayAL 1 SelectFieldChoice) -- use `duet` / `duet'` to create

data MultiChoiceField
  = SelectMultipleField (ArrayAL 1 SelectFieldChoice) -- use `solo` / `solo'` to create
  | CheckboxField (ArrayAL 1 SelectFieldChoice) -- use `duet` / `duet'` to create

data Field
  = SingleChoiceField SingleChoiceField
  | MultiChoiceField MultiChoiceField
  | InputField
  | TextArea

type SingleChoiceFieldProps =
  { initialValue :: String
  , onValueChange :: String -> Effect Unit
  , type :: SingleChoiceField
  }

-- FIXME: These widgets were create before we started to bind to `react-bootstrap` and implementation
-- `useForm` so they should be deprecated and replaced.
-- Choice widget either radio button or checkbox which can handle
-- multiple options.
mkSingleChoiceField :: Effect (SingleChoiceFieldProps -> JSX)
mkSingleChoiceField = do
  widgetPrefix <- random <#> \n -> "single-choice-widget-" <> show n <> "-"
  counterRef <- Ref.new 0

  component "SingleChoiceField" \{ initialValue, onValueChange, type: type_ } -> React.do
    currValue /\ setValue <- useState initialValue
    possibleIdPrefix /\ setPossibleId <- useState Nothing
    let
      onChange :: String -> Effect Unit
      onChange value = do
        onValueChange value
        setValue (const $ value)

    useEffectOnce do
      counter <- Ref.read counterRef
      Ref.write (counter + 1) counterRef
      setPossibleId (const $ Just $ widgetPrefix <> show counter <> "-")
      pure (pure unit)

    let
      jsx :: JSX
      jsx = case type_ of
        SelectField choices -> do
          let
            choices' = ArrayAL.toArray choices

            options :: Array JSX
            options = choices' <#> \(value /\ label /\ disabled) ->
              DOM.option { value, disabled } [ R.text label ]
          DOM.select
            { className: "form-control"
            , value: currValue
            , onChange: handler targetValue $ traverse onChange >>> void
            }
            options
        RadioButtonField choices -> do
          let
            choices' = ArrayAL.toArray choices

          case possibleIdPrefix of
            Just idPrefix -> R.div_ $ do
              let
                -- Single radio doesn't trigger onChange when it's already selected
                inputType = if length choices' == 1 then "checkbox" else "radio"
              choices' `flip mapWithIndex` \idx (value /\ label /\ disabled) -> do
                let
                  id = idPrefix <> show idx
                  checked = currValue == value
                DOM.div { className: "form-check form-switch text-start my-3" }
                  [ R.input
                      { className: "form-check-input"
                      , type: inputType
                      , checked
                      , id: id
                      -- This `if` covers single checkbox case
                      , onChange: handler_
                          if checked then onChange ""
                          else onChange value
                      , disabled
                      , name: "radio"
                      }
                  , DOM.label
                      { className: "form-check-label text-start", htmlFor: id }
                      [ label :: JSX ]
                  ]
            Nothing -> mempty
    pure jsx

-- | A checkbox which handles single boolean choice
mkBooleanField
  :: Effect
       ( { disabled :: Boolean
         , initialValue :: Boolean
         , label :: JSX
         , onToggle :: Boolean -> Effect Unit
         }
         -> JSX
       )
mkBooleanField = do
  singleChoiceField <- mkSingleChoiceField
  pure \{ initialValue, onToggle, disabled, label } -> singleChoiceField
    { initialValue: if initialValue then "on" else ""
    , onValueChange: \value ->
        onToggle (value == "on")
    , type: RadioButtonField $ ArrayAL.solo ("on" /\ label /\ disabled)
    }

