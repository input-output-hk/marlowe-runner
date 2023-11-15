module Contrib.ReactBootstrap.FormSpecBuilders.StatefulFormSpecBuilders where

import Prelude

import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Polyform.Batteries.BigInt as Batteries.BigInt
import Contrib.Polyform.FormSpecBuilder (FormSpecBuilderT, formSpecBuilder)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatefulFormSpec (StatefulFormSpec, liftStatelessFormSpec)
import Contrib.Polyform.FormSpecs.StatefulFormSpec as StatefulFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceConfig, ChoiceFieldChoices(..), ChoiceFieldOptionalProps, ChoiceFieldProps, FieldChoice, TextInputOptionalProps, TextInputProps, UseChoiceField(..), _genFieldId, defaultChoiceFieldProps, defaultTextInputProps, optV, renderChoiceField, renderTextInput, requiredV')
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (StateT)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime (DateTime)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Polyform.Batteries as Batteries
import Polyform.Batteries.Int as Batteries.Int
import Polyform.Validator (liftFnMMaybe)
import Prim.Row as Row
import React.Basic (JSX)
import Record as Record
import Type.Prelude (Proxy(..))

type StatefulBootstrapFormSpec validatorM st = StatefulFormSpec validatorM st (Array JSX) String

textInput
  :: forall a builderM props validatorM st
   . Monad builderM
  => Monad validatorM
  => Defaults TextInputOptionalProps { | props } (TextInputProps (StateT st validatorM) a)
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query a
textInput props = formSpecBuilder do
  let
    props' = defaults defaultTextInputProps props
  name <- _genFieldId props'
  let
    form = StatefulFormSpec.input
      name
      props'.initial
      ( \_ -> Array.singleton
          <<< renderTextInput
            { "aria-label": props'."aria-label"
            , "aria-labelledby": props'."aria-labelledby"
            , role: props'.role
            , layout: props'.layout
            , possibleLabel: props'.label
            , possibleHelpText: props'.helpText
            , name
            , placeholder: props'.placeholder
            , "type": props'."type"
            , max: props'.max
            , min: props'.min
            , sizing: props'.sizing
            , step: props'.step
            , extraClassName: NoProblem.toMaybe props'.inputExtraClassName
            }
      )
      props'.touched
      props'.validator
  pure form

_validator = (Proxy :: Proxy "validator")
_type = (Proxy :: Proxy "type")

_typedTextInput
  :: forall a builderM props st validatorM
   . Monad builderM
  => Monad validatorM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) a | props }
       (TextInputProps (StateT st validatorM) a)
  => { | props }
  -> String
  -> Batteries.Validator (StateT st validatorM) String (Maybe String) a
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query a
-- (StatelessBootstrapFormSpec validatorM) Query a
_typedTextInput props type_ validator = do
  textInput props''
  where
  props'' = Record.insert _validator validator $ Record.insert _type type_ $ props

intInput
  :: forall builderM props st validatorM
   . Monad validatorM
  => Monad builderM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) Int | props }
       (TextInputProps (StateT st validatorM) Int)
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query Int
-- (Stat validatorM) Query Int
intInput props = _typedTextInput props "number" $ requiredV' validator
  where
  validator :: Batteries.Validator (StateT st validatorM) String String Int
  validator = Batteries.stringifyValidator Batteries.Int.validator

optIntInput
  :: forall builderM props st validatorM
   . Monad validatorM
  => Monad builderM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) (Maybe Int) | props }
       (TextInputProps (StateT st validatorM) (Maybe Int))
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query (Maybe Int)
optIntInput props = _typedTextInput props "number" $ optV validator
  where
  validator :: Batteries.Validator (StateT st validatorM) String String Int
  validator = Batteries.stringifyValidator Batteries.Int.validator

bigIntInput
  :: forall builderM props st validatorM
   . Monad validatorM
  => Monad builderM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) BigInt | props }
       (TextInputProps (StateT st validatorM) BigInt)
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query BigInt
bigIntInput props = _typedTextInput props "number" $ requiredV' validator
  where
  validator :: Batteries.Validator (StateT st validatorM) String String BigInt
  validator = Batteries.stringifyValidator Batteries.BigInt.validator

optBigIntInput
  :: forall builderM props st validatorM
   . Monad validatorM
  => Monad builderM
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps
       { "type" :: String, validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) (Maybe BigInt) | props }
       (TextInputProps (StateT st validatorM) (Maybe BigInt))
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query (Maybe BigInt)
optBigIntInput props = _typedTextInput props "number" $ optV validator
  where
  validator :: Batteries.Validator (StateT st validatorM) String String BigInt
  validator = Batteries.stringifyValidator Batteries.BigInt.validator

dateTimeField
  :: forall a builderM validatorM st
   . Monad builderM
  => Monad validatorM
  => Maybe JSX
  -> Maybe JSX
  -> Batteries.Validator validatorM String (Maybe DateTime) a
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query a
dateTimeField possibleLabel possibleHelpText dateTimeValidator = do
  FormSpecBuilder.hoistFormSpec liftStatelessFormSpec $ StatelessFormSpecBuilders.dateTimeField possibleLabel possibleHelpText dateTimeValidator

type MultiFieldIds = { multi :: FieldId, sub :: Array FieldId }

choiceField
  :: forall a builderM props st validatorM
   . Monad validatorM
  => Monad builderM
  => Defaults ChoiceFieldOptionalProps { | props } (ChoiceFieldProps (StateT st validatorM) a)
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query a
choiceField props = formSpecBuilder do
  name <- _genFieldId props'
  let
    -- input name initial render touched validator = Form
    form = StatefulFormSpec.input
      name
      props'.initial
      -- props'.missingError
      ( \_ -> renderChoiceField
          { inline: props'.inline
          , disabled: props'.disabled
          , possibleLabel: props'.label
          , name
          , choices: props'.choices
          , possibleHelpText: props'.helpText
          , showValidity: props'.showValidity
          }
      )
      props'.touched
      props'.validator
  pure form
  where
  props' = defaults defaultChoiceFieldProps props

choiceFieldFromCustomRange
  :: forall a builderM props props' st validatorM
   . Monad validatorM
  => Monad builderM
  => Defaults ChoiceFieldOptionalProps { | props' } (ChoiceFieldProps (StateT st validatorM) a)
  => Row.Nub
       ( choices :: ChoiceFieldChoices
       , initial :: String
       , validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) a
       | props
       )
       props'
  => UseChoiceField a
  -> (ArrayAL 1 a)
  -> { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query a
choiceFieldFromCustomRange useElement arr props = do
  let
    validator :: Batteries.Validator (StateT st validatorM) String (Maybe String) a
    validator = do
      let
        value2Choice = Map.fromFoldable $ mapWithIndexFlipped arr \idx choice -> show idx /\ choice
      liftFnMMaybe (\v -> pure [ "Invalid choice: " <> show v ]) \possibleIdx -> runMaybeT do
        choice <- MaybeT $ pure do
          idx <- possibleIdx
          Map.lookup idx value2Choice
        -- liftEffect $ setPartialFormResult $ Just choice
        pure choice

    asChoice :: forall doc. (a -> ChoiceConfig doc) -> Int -> a -> FieldChoice doc
    asChoice mkCfg idx a = do
      let
        value = show idx

        cfg :: ChoiceConfig doc
        cfg = mkCfg a

      { label: cfg.label
      , value
      , disabled: cfg.disabled
      , helpText: cfg.helpText
      }

    initial = "0"

    choices = case useElement of
      UseRadioButtonField mkCfg -> RadioButtonFieldChoices
        { switch: true
        , choices: do
            let
              asChoice' = asChoice mkCfg
            mapWithIndex asChoice' arr
        }
      UseSelectField mkCfg -> SelectFieldChoices do
        let
          asChoice' = asChoice mkCfg
        mapWithIndex asChoice' arr

    props' :: { | props' }
    props' = Record.merge
      { choices
      , validator
      , initial
      }
      props
  choiceField props'

