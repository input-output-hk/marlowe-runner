module Contrib.ReactBootstrap.FormBuilder where

import Prelude

import Data.Array.ArrayAL as ArrayAL
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (FieldId(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Undefined.NoProblem (Opt, fromOpt)
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem
import Debug (traceM)
import Polyform.Validator (liftFn, liftFnEither)
import React.Basic (JSX, fragment)
import React.Basic.DOM.Simplified.Generated as S
import React.Basic.Events (handler_)
import React.Basic.Hooks.UseForm (InputState)
import React.Basic.Hooks.UseForm as UseForm
import ReactBootstrap.Form as Form
import ReactBootstrap.Form.Check as Check
import ReactBootstrap.FormBuilder (ChoiceFieldChoices(..), FormBuilderM, FormBuilderT', FormElement, fieldValidity, formBuilderT, genId, radioFieldChoice, renderChoiceField, renderPossibleHelpText)


type BooleanFieldPropsRow r =
  ( switch :: Opt Boolean
  , label :: JSX
  , helpText :: Opt JSX
  , inline :: Opt Boolean
  , name :: Opt FieldId
  , initial :: Opt Boolean
  , touched :: Opt Boolean
  | r
  )

-- The same as above but insead of Maybe we use Opt
genFieldId
  :: forall builderM r
   . Monad builderM
  => { name :: Opt FieldId | r }
  -> FormBuilderM builderM FieldId
genFieldId props = do
  case NoProblem.toMaybe props.name of
    Just name -> pure name
    Nothing  -> FieldId <$> genId


booleanField
  :: forall builderM props validatorM
   . Monad validatorM
  => Monad builderM
  => NoProblem.Coerce { | props } { | BooleanFieldPropsRow () }
  => { | props }
  -> FormBuilderT' builderM validatorM Boolean
booleanField props = do
  let
    props' :: { | BooleanFieldPropsRow () }
    props' = NoProblem.coerce props
  formBuilderT do
    name <- genFieldId props'
    let
      radioChoice = radioFieldChoice "on" props'.label
      -- | Value is artificial in this case because
      -- | the plain precense
      validator = liftFnEither case _ of
        "on" -> Right true
        _ -> Right false
      booleanAsValue = if _
        then "on"
        else "off"

      initial = fromOpt false props'.initial
      switch = fromOpt true props'.switch

      -- input name initial render touched validator = Form
      form = UseForm.optInput
        name
        (booleanAsValue initial)
        ( renderBooleanField
            { disabled: false
            , inline: fromOpt false props'.inline
            , name
            , label: props'.label
            , possibleHelpText: NoProblem.toMaybe props'.helpText
            , switch
            }
        )
        (fromOpt false props'.touched)
        validator
    pure $ do
      let
        emptyAsFalse = liftFn case _ of
          Just value -> value
          Nothing -> false
      form >>> UseForm.liftValidator emptyAsFalse


renderBooleanField
  :: { disabled :: Boolean
     , inline :: Boolean
     , label :: JSX
     , name :: FieldId
     , possibleHelpText :: Maybe JSX
     , switch :: Boolean
     }
  -> InputState String
  -> Array FormElement
renderBooleanField { disabled, inline, label, possibleHelpText, name, switch } { value: selectedValue, errors, onChange, touched } = do
  let
    nameStr = un FieldId name
    label' =
      if inline then S.label {} [ label ]
      else S.label { className: "col-sm-3 col-form-label-sm", htmlFor: nameStr } [ label ]

    body = do
      let
        className =
          if not inline then "form-check col-sm-9"
          else "form-check"
        checked = "on" == selectedValue
        { isValid, isInvalid } = fieldValidity touched "on" errors
        helpText = renderPossibleHelpText possibleHelpText

      S.div { className } $
        [ Form.check
          { className: "min-h-1_2rem"
          , disabled
          , id: nameStr
          , isValid
          , isInvalid
          , name: nameStr
          , "type":
              if switch then Check.checkType.switch
              else Check.checkType.radio
          , value: "on"
          , checked
          , onChange: handler_ do
              onChange $ if checked then "off" else "on"
          }
          -- We should probably render this as `feedback` above when in `inline`
        , helpText
        ]

  pure $
    if inline then S.div { className: "col-12 flex-fill" } [ label', body ]
    else S.div { className: "row mb-2" } [ label', body ]
