module Contrib.ReactBootstrap.FormSpecBuilders.StatefulFormSpecBuilders where

import Prelude

import Contrib.Polyform.FormSpecBuilder (FieldIdPrefix(..), FormSpecBuilderM, FormSpecBuilderT, _fullPrefix, formSpecBuilderT, genId, unFormSpecBuilder)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatefulFormSpec (InputState, StatefulFormSpec(..), liftStatelessFormSpec)
import Contrib.Polyform.FormSpecs.StatefulFormSpec as StatefulFormSpec
import Contrib.Polyform.FormSpecs.StatelessFormSpec (StatelessFormSpec(..))
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (TextInputOptionalProps, TextInputProps, _genFieldId, defaultTextInputProps, renderPossibleHelpText, renderTextInput)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders as StatelessFormSpecBuilders
import Control.Alternative as Alternative
import Control.Monad.Reader (withReaderT)
import Control.Monad.State (StateT)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Bifunctor (bimap, lmap)
import Data.Date (Date)
import Data.DateTime (DateTime(..), Hour, Minute, Time(..))
import Data.DateTime.ISO (parseISODate)
import Data.Decimal (Decimal)
import Data.Either (Either(..), either, note)
import Data.Enum (class BoundedEnum, toEnum, upFromIncluding)
import Data.Foldable (fold, foldMap, null, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as FormURLEncoded
import Data.Formatter.Parser.Number (parseDigit)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid as Monoid
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right)
import Data.String as String
import Data.Tuple (fst)
import Data.Undefined.NoProblem (Opt, fromOpt)
import Data.Undefined.NoProblem (toMaybe, undefined) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem
import Parsing (Parser, runParser) as Parsing
import Parsing (fail)
import Parsing.Combinators (optional, try) as Parsing
import Parsing.String (char) as Parsing
import Polyform (Validator)
import Polyform.Batteries as Batteries
import Polyform.Batteries.Decimal as Batteries.Decimal
import Polyform.Batteries.Int as Batteries.Int
import Polyform.Batteries.Number as Batteries.Number
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Duals as UrlEncoded.Duals
import Polyform.Batteries.UrlEncoded.Types.Errors (ErrorId(..), Errors(..))
import Polyform.Dual as Polyform.Dual
import Polyform.Validator (liftFnEither)
import Polyform.Validator as Validator
import Polyform.Validator.Dual as Polyform.Validator.Dual
import Prim.Row as Row
import React.Basic (JSX)
import React.Basic.DOM (div_, li_, text, ul_) as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (type (/\), (/\))
import ReactBootstrap.Form as Bootstrap.Form
import ReactBootstrap.Form as Form
import ReactBootstrap.Form.Check as Check
import ReactBootstrap.Form.Control as Form.Control
import Record as Record
import Type.Prelude (Proxy(..))


type StatefulBootstrapFormSpec validatorM st = StatefulFormSpec validatorM st (Array JSX) String

fromStatlessFormSpec = FormSpecBuilder.hoistFormSpec liftStatelessFormSpec

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
          { layout: props'.layout
          , possibleLabel: props'.label
          , possibleHelpText: props'.helpText
          , name
          , placeholder: props'.placeholder
          , "type": props'."type"
          , max: props'.max
          , min: props'.min
          , sizing: props'.sizing
          , step: props'.step
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
