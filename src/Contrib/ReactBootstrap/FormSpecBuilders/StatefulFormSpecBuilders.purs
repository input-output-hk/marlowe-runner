module Contrib.ReactBootstrap.FormSpecBuilders.StatefulFormSpecBuilders where

import Prelude

import Contrib.Polyform.FormSpecBuilder (FormSpecBuilderT, formSpecBuilderT)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatefulFormSpec (StatefulFormSpec, liftStatelessFormSpec)
import Contrib.Polyform.FormSpecs.StatefulFormSpec as StatefulFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (TextInputOptionalProps, TextInputProps, _genFieldId, defaultTextInputProps, renderTextInput)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Control.Monad.State (StateT)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.FormURLEncoded.Query (FieldId, Query)
import Data.Maybe (Maybe)
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Polyform.Batteries as Batteries
import React.Basic (JSX)

type StatefulBootstrapFormSpec validatorM st = StatefulFormSpec validatorM st (Array JSX) String

fromStatelessFormSpec = FormSpecBuilder.hoistFormSpec liftStatelessFormSpec

textInput
  :: forall a builderM props validatorM st
   . Monad builderM
  => Monad validatorM
  => Defaults TextInputOptionalProps { | props } (TextInputProps (StateT st validatorM) a)
  => { | props }
  -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) Query a
textInput props = formSpecBuilderT do
  let
    props' = defaults defaultTextInputProps props
  name <- _genFieldId props'
  let
    form = StatefulFormSpec.input
      name
      props'.initial
      ( Array.singleton <<< renderTextInput
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

-- multiField
--   :: forall builderM i o validatorM st
--    . Monad builderM
--   => Maybe JSX
--   -> Maybe JSX
--   -> (FieldId -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) i o)
--   -> FormSpecBuilderT builderM (StatefulBootstrapFormSpec validatorM st) i o
-- multiField possibleLabel possibleHelpText fieldsFormBuilder = formSpecBuilderT do
--   prefix <- FieldIdPrefix <$> genId
--   form <- withReaderT (const $ Just $ prefix) do
--     let
--       prefixStr = _fullPrefix prefix
--       multiFieldId = FieldId prefixStr
--     unFormSpecBuilder (fieldsFormBuilder multiFieldId)
--   let
--     StatefulFormSpec (formRecord@{ render }) = form
--     prefixPattern = String.Pattern (_fullPrefix prefix)
--     isPrefixOf p = isJust <<< String.stripPrefix p
--     matchesPrefix = isPrefixOf prefixPattern
-- 
--   -- errorId = Safe.Coerce.coerce fieldId
--   pure $ StatefulFormSpec formRecord
--     { render = \state -> do
--         let
--           possibleFieldErrors :: Maybe (Map ErrorId (Array String))
--           possibleFieldErrors = do
--             errors /\ validationQuery <- (state.errors :: Maybe (UrleEncoded.Errors String /\ Query))
--             let
--               filterKeyFn :: forall n. Newtype n String => n -> Boolean
--               filterKeyFn key = matchesPrefix (unwrap key)
-- 
--               stateSubquery = Map.filterKeys filterKeyFn (un FormURLEncoded.Query state.query)
--               validationSubquery = Map.filterKeys filterKeyFn (un FormURLEncoded.Query validationQuery)
--             Alternative.guard (stateSubquery == validationSubquery) $> do
--               let
--                 wasTouched (ErrorId fieldId) = fromMaybe false $ do
--                   { touched: Disj touched } <- Map.lookup (FieldId fieldId) state.fields
--                   pure touched
--               Map.filterKeys ((&&) <$> wasTouched <*> filterKeyFn) (un Errors errors)
-- 
--           label =
--             DOM.label
--               { className: "col-sm-3 col-form-label-sm" } $ fold possibleLabel
-- 
--           body =
--             DOM.div { className: "col-sm-9" } do
--               [ DOM.div { className: "row row-cols-lg-auto align-items-center" }
--                   $ render state
--               , renderPossibleHelpText possibleHelpText
--               ]
--               <> flip foldMap possibleFieldErrors \fieldErrors ->
--                 flip foldMapWithIndex fieldErrors \fieldId errors -> do
--                   let
--                     errorId = un ErrorId fieldId
--                   DOM.div
--                     { className: "col-sm-9 offset-sm-3" }
--                     [ DOM.div
--                         { className: "invalid-feedback d-block"
--                         , id: errorId
--                         }
--                         $ map (DOOM.div_ <<< Array.singleton <<< DOOM.text) errors
--                     ]
--         [ DOM.div { className: "row mb-lg-2 mb-md-1" } [ label, body ] ]
--     }
