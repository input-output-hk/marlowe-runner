module Component.ApplyInputs.Forms where

import Prelude

import Component.InputHelper (ChoiceInput(..))
import Contrib.Data.Foldable (foldMapWithIndexFlipped)
import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Polyform.FormSpecBuilder (FormSpecBuilder, evalBuilder', formSpecBuilder, unFormSpecBuilder)
import Contrib.Polyform.FormSpecs.StatefulFormSpec (RenderFn, RenderInputFn, StatefulFormSpec(..), liftValidator, toFormRender)
import Contrib.ReactBootstrap.FormSpecBuilders.StatefulFormSpecBuilders (bigIntInput, choiceField, choiceFieldFromCustomRange)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (ChoiceFieldChoices(..), FieldLayout(..), UseChoiceField(..), selectFieldChoice)
import Control.Monad.State (StateT, get, put)
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int.AtLeast as Int.ArrayAL
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.TraversableWithIndex (forWithIndex)
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Language.Marlowe.Core.V1.Semantics.Types (Bound(..), ChoiceId(..), InputContent(..)) as V1
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded.Types.Errors (ErrorId(..))
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Validator (liftFn, liftFnM, liftFnMEither, liftFnMaybe, runValidator)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, (/\))
import ReactBootstrap.Form as Form
import ReactBootstrap.Form.Check as Check
import Runner.Contrib.Data.Array as Array
import Type.Prelude (Proxy(..))

-- Choices can be repeated by name in the `When` clause (with different ranges)
-- so we want use indexing to disambiguate them.
newtype ChoiceIdx = ChoiceIdx String

derive instance Newtype ChoiceIdx _
derive newtype instance Eq ChoiceIdx
derive newtype instance Ord ChoiceIdx

newtype ChoiceName = ChoiceName String

derive instance Newtype ChoiceName _
derive newtype instance Eq ChoiceName
derive newtype instance Ord ChoiceName

type ExtraFormState = Maybe ChoiceIdx

type FormChoiceFields = { choiceField :: Array JSX, valueField :: Array JSX }

mkInputsValuesForms
  :: forall validatorM
   . Monad validatorM
  => NonEmptyArray ChoiceInput
  -> FormSpecBuilder (StatefulFormSpec validatorM ExtraFormState (ChoiceIdx -> FormChoiceFields) String) Query (Maybe BigInt)
mkInputsValuesForms choiceInputs = do
  let
    flattenValue = formSpecBuilder $ pure $ liftValidator $ liftFn \res -> do
      let
        step (Just v) _ = Just v
        step Nothing accum = accum
      foldr step Nothing res

  flattenValue <<< forWithIndex choiceInputs \idx (ChoiceInput (V1.ChoiceId _ _) bounds _) -> do
    let
      choiceIdx = ChoiceIdx (show idx)
      twenty = BigInt.fromInt 20

      possibleValueChoices :: Maybe (ArrayAL 1 BigInt)
      possibleValueChoices = case bounds of
        [ V1.Bound min max ] | BigInt.abs (min - max) < twenty -> ArrayAL.fromArray (Proxy :: Proxy 1) (Array.rangeBigInt min max)
        _ -> Nothing
      formSpecBuilderT = case possibleValueChoices of
        -- -- We should use even `2` here because for `1` element we want to use `hidden` field;-)
        --   [ V1.Bound min max ] | min == max -> Nothing
        Just valueChoices -> do
          let
            fieldDisabled = Int.ArrayAL.toInt (ArrayAL.length valueChoices) == 1
          choiceFieldFromCustomRange
            (UseSelectField $ \i -> { disabled: false, label: BigInt.toString i, helpText: Nothing })
            valueChoices
            { disabled: fieldDisabled, touched: false, inline: true }
        Nothing -> do
          bigIntInput
            { role: NoProblem.opt "textarea"
            , "aria-label": NoProblem.opt "choice-input"
            , layout: Inline
            }

      formSpecBuilderT' = formSpecBuilder do
        StatefulFormSpec spec <- unFormSpecBuilder formSpecBuilderT
        let
          validator' = liftFnMEither \i -> do
            get >>= case _ of
              Just choiceIdx' | choiceIdx == choiceIdx' -> do
                V res <- runValidator spec.validator i
                pure $ Just <$> res
              _ -> pure $ Right Nothing

          render :: RenderFn (state :: ExtraFormState) String (ChoiceIdx -> FormChoiceFields)
          render formState = eq choiceIdx >>>
            if _ then (mempty :: FormChoiceFields) { valueField = spec.render formState }
            else mempty
        pure $ StatefulFormSpec $ spec { render = render, validator = validator' }
    formSpecBuilderT'

-- I want to create a custom renderer which:
-- * when provided a choiceID render a radio button
mkChoiceRender
  :: NonEmptyArray ChoiceInput
  -> RenderInputFn String ExtraFormState (ChoiceIdx -> Array JSX)
mkChoiceRender choiceInputs _ { value: selectedChoiceIdxStr, name: fieldId, onChange } choiceIdx = do
  let
    choiceIdx2ChoiceId = Map.fromFoldable $ mapWithIndexFlipped choiceInputs \idx (ChoiceInput choiceId _ _) ->
      ChoiceIdx (show idx) /\ choiceId
    fieldIdStr = un FieldId fieldId

    selectedChoiceIdx = ChoiceIdx selectedChoiceIdxStr
    value = un ChoiceIdx choiceIdx
    checked = choiceIdx == selectedChoiceIdx
    label = DOOM.text $ fromMaybe "" do
      (V1.ChoiceId choiceName _) <- Map.lookup choiceIdx choiceIdx2ChoiceId
      pure choiceName

  pure $ Form.check
    { id: fieldIdStr <> "-" <> value
    , label
    , name: fieldIdStr
    , "type": Check.checkType.switch
    , value
    , checked
    , onChange: handler_ do
        when (not checked) $ onChange value
    }

mkInputChoiceField
  :: forall validatorM
   . Monad validatorM
  => FieldId
  -> NonEmptyArray ChoiceInput
  -> FormSpecBuilder (StatefulFormSpec validatorM ExtraFormState (ChoiceIdx -> FormChoiceFields) String) Query V1.ChoiceId
mkInputChoiceField fieldId choiceInputs = formSpecBuilder do
  let
    choices = SelectFieldChoices do
      let
        toChoice idx (ChoiceInput (V1.ChoiceId name _) _ _) = do
          selectFieldChoice name (show idx)
      ArrayAL.fromNonEmptyArray $ mapWithIndex toChoice choiceInputs

    value2ChoiceName = Map.fromFoldable $ mapWithIndexFlipped choiceInputs \idx choiceInput ->
      show idx /\ choiceInput

    validator :: Batteries.Validator (StateT ExtraFormState validatorM) String (Maybe String) V1.ChoiceId
    validator = do
      let
        validateChoice = liftFnMaybe (\v -> [ "Invalid choice: " <> show v ]) \possibleIdx -> do
          idx <- possibleIdx
          (ChoiceInput choiceId _ _) <- Map.lookup idx value2ChoiceName
          pure (ChoiceIdx idx /\ choiceId)
        storeChoiceId = liftFnM \(choiceId /\ choiceName) -> do
          put (Just choiceId)
          pure choiceName
      storeChoiceId <<< validateChoice

    render st cId = do
      let
        choiceField = toFormRender fieldId (mkChoiceRender choiceInputs) st cId
      (mempty :: FormChoiceFields) { choiceField = choiceField }
  StatefulFormSpec spec <- unFormSpecBuilder $ choiceField { choices, validator, touched: true, initial: "0", name: Just fieldId }
  pure $ StatefulFormSpec $ spec { render = render }

mkApplyInputForm
  :: forall m
   . Monad m
  => NonEmptyArray ChoiceInput
  -> StatefulFormSpec m (Maybe ChoiceIdx) (Array JSX) String Query V1.InputContent
mkApplyInputForm choiceInputs = do
  let
    collectResult = liftValidator $ liftFnMaybe (\_ -> Errors.singleton (ErrorId "form") [ "Missing choice value" ]) \{ choiceId, possibleChoiceValue } -> do
      choiceValue <- possibleChoiceValue
      pure $ V1.IChoice choiceId choiceValue
    StatefulFormSpec formSpec = collectResult <<< evalBuilder' ado
      choiceId <- mkInputChoiceField (FieldId "input-choices") choiceInputs
      possibleChoiceValue <- mkInputsValuesForms choiceInputs
      in
        { choiceId, possibleChoiceValue }

    render :: RenderFn (state :: ExtraFormState) String (Array JSX) -- (Maybe ChoiceIdx -> (Array JSX))
    render formState = do
      let
        renderField choiceIdx = do
          let
            { choiceField, valueField } = formSpec.render formState choiceIdx
          pure $ DOM.div { className: "row mb-2" }
            [ DOM.div { className: "col-6" } choiceField
            , DOM.div { className: "col-6" } valueField
            ]

      foldMapWithIndexFlipped choiceInputs \idx _ -> do
        (if idx /= 0 then [ DOOM.hr {} ] else mempty)
          <> renderField (ChoiceIdx $ show idx)
    formSpec' = formSpec { render = render }
  StatefulFormSpec formSpec'

