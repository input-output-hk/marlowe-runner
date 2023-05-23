module Component.CreateContract.SecondStep where

import Prelude

import Actus.Domain (CT(..), ContractTerms)
import Actus.Domain as A
import Actus.Domain.ContractTerms (mkContractTerms)
import Component.CreateContract.Types (AmortizingLoanChoice(..), ContractFormTypeChoice(..))
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, ActusDictionaries)
import Component.Widgets (link)
import Contrib.Data.Foldable (foldMapWithIndexFlipped)
import Contrib.Data.FunctorWithIndex (mapWithIndexFlipped)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.Polyform.Validator (liftMaybe)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, ChoiceFieldChoices(..), FormBuilder, FormBuilder', FormBuilderT, FormBuilderT', UseChoiceField(..), choiceField, choiceField', dateInput, dateTimeField, decimalInput, formBuilderT, intInput, liftBuilderM, multiField, selectFieldChoice, timeInput, unFormBuilder)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Alternative as Alternative
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, decodeJson, parseJson)
import Data.Argonaut as A
import Data.Argonaut as Argonaut
import Data.Array ((..))
import Data.Array as Array
import Data.Array.ArrayAL as ArrayAL
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Foldable (fold, foldr, for_, length)
import Data.FoldableWithIndex (forWithIndex_)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (opt)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Foreign.Object
import Foreign.Object as Object
import Polyform.Batteries as Batteries
import Polyform.Batteries.Int as Batteries.Int
import Polyform.Validator (liftFnEither) as Validator
import Polyform.Validator (liftFnMaybe)
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (br, div_, text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component)
import React.Basic.Hooks as React

amortizingLoanChoiceToLabel :: AmortizingLoanChoice -> String
amortizingLoanChoiceToLabel = case _ of
  PrincipalAtMaturity -> "Principal at maturity"
  LinearAmortizer -> "Linear amortizer"
  NegativeAmortizer -> "Negative amortizer"
  Annuity -> "Annuity"

amortizingLoanActusDictLabel :: AmortizingLoanChoice -> String
amortizingLoanActusDictLabel = case _ of
  PrincipalAtMaturity -> "principalAtMaturity"
  LinearAmortizer -> "linearAmortizer"
  NegativeAmortizer -> "negativeAmortizer"
  Annuity -> "annuity"

amortizingLoanToContractType :: AmortizingLoanChoice -> CT
amortizingLoanToContractType = case _ of
  PrincipalAtMaturity -> PAM
  LinearAmortizer -> LAM
  NegativeAmortizer -> NAM
  Annuity -> ANN

businessDayConventionChoiceToLabel :: A.BDC -> String
businessDayConventionChoiceToLabel = case _ of
  A.BDC_NULL -> "No shift"
  A.BDC_SCF -> "Shift/calculate following"
  A.BDC_SCMF -> "Shift/calculate modified following"
  A.BDC_CSF -> "Calculate/shift following"
  A.BDC_CSMF -> "Calculate/shift modified following"
  A.BDC_SCP -> "Shift/calculate preceding"
  A.BDC_SCMP -> "Shift/calculate modified preceding"
  A.BDC_CSP -> "Calculate/shift preceding"
  A.BDC_CSMP -> "Calculate/shift modified preceding"

businessDayConventionField :: forall m. Monad m => FormBuilderT' m Effect A.BDC
businessDayConventionField = do
  let
    choiceConfig a =
      { label: businessDayConventionChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    Nothing
    { label: Just $ DOOM.text "Business day convention" }

calendarChoiceToLabel :: A.Calendar -> String
calendarChoiceToLabel = case _ of
  A.CLDR_NC -> "No calendar"
  A.CLDR_MF -> "Monday to Friday"

endOfMonthConventionChoiceToLabel :: A.EOMC -> String
endOfMonthConventionChoiceToLabel = case _ of
  A.EOMC_EOM -> "End of month"
  A.EOMC_SD -> "Same day"

-- -- |ContractRole
-- data CR
--   = CR_RPA -- ^ Real position asset
--   | CR_RPL -- ^ Real position liability
--   | CR_CLO -- ^ Role of a collateral
--   | CR_CNO -- ^ Role of a close-out-netting
--   | CR_COL -- ^ Role of an underlying to a collateral
--   | CR_LG -- ^ Long position
--   | CR_ST -- ^ Short position
--   | CR_BUY -- ^ Protection buyer
--   | CR_SEL -- ^ Protection seller
--   | CR_RFL -- ^ Receive first leg
--   | CR_PFL -- ^ Pay first leg
--   | CR_RF -- ^ Receive fix leg
--   | CR_PF -- ^ Pay fix leg
contractRoleChoiceToLabel :: A.CR -> String
contractRoleChoiceToLabel = case _ of
  A.CR_RPA -> "Real position asset"
  A.CR_RPL -> "Real position liability"
  A.CR_CLO -> "Role of a collateral"
  A.CR_CNO -> "Role of a close-out-netting"
  A.CR_COL -> "Role of an underlying to a collateral"
  A.CR_LG -> "Long position"
  A.CR_ST -> "Short position"
  A.CR_BUY -> "Protection buyer"
  A.CR_SEL -> "Protection seller"
  A.CR_RFL -> "Receive first leg"
  A.CR_PFL -> "Pay first leg"
  A.CR_RF -> "Receive fix leg"
  A.CR_PF -> "Pay fix leg"

-- contractRoleField :: forall m. Monad m => FormBuilderT' m Effect A.CR
contractRoleField = formBuilderT do
  let
    choiceConfig a =
      { label: contractRoleChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  helpText <- lift $ asks $ _.terms >>> getActusDescription "contractRole"
  unFormBuilder $ choiceField'
    (UseSelectField choiceConfig)
    (Just $ ArrayAL.solo' A.CR_RPA [ A.CR_RPL ])
    { label: Just $ DOOM.text "Contract role"
    , helpText: DOOM.text <$> helpText
    }

-- |DayCountConvention
-- data DCC
--   = DCC_A_AISDA -- ^ Actual/Actual ISDA
--   | DCC_A_360 -- ^ Actual/360
--   | DCC_A_365 -- ^ Actual/365
--   | DCC_E30_360ISDA -- ^ 30E/360 ISDA
--   | DCC_E30_360 -- ^ 30E/360
--   | DCC_B_252 -- ^ Business / 252
dayCountConventionChoiceToLabel :: A.DCC -> String
dayCountConventionChoiceToLabel = case _ of
  A.DCC_A_AISDA -> "Actual/Actual ISDA"
  A.DCC_A_360 -> "Actual/360"
  A.DCC_A_365 -> "Actual/365"
  A.DCC_E30_360ISDA -> "30E/360 ISDA"
  A.DCC_E30_360 -> "30E/360"
  A.DCC_B_252 -> "Business / 252"

-- dayCountConventionField :: forall m. Monad m => FormBuilderT' m Effect A.DCC
dayCountConventionField = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription "dayCountConvention"
  let
    choiceConfig a =
      { label: dayCountConventionChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  unFormBuilder $ choiceField'
    (UseSelectField choiceConfig)
    Nothing
    { label: Just $ DOOM.text "Day count convention"
    , helpText: DOOM.text <$> helpText
    }

-- FIXME: Implemnt cycle widget
-- -- |CyclePeriod
-- data Period
--   = P_D -- ^ Day
--   | P_W -- ^ Week
--   | P_M -- ^ Month
--   | P_Q -- ^ Quarter
--   | P_H -- ^ Half year
--   | P_Y -- ^ Year
cyclePeriodChoiceToLabel :: A.Period -> String
cyclePeriodChoiceToLabel = case _ of
  A.P_D -> "Day"
  A.P_W -> "Week"
  A.P_M -> "Month"
  A.P_Q -> "Quarter"
  A.P_H -> "Half year"
  A.P_Y -> "Year"

--- cyclePeriodField :: _ -> FormBuilder' Effect A.Period
cyclePeriodField props = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription "dayCountConvention"
  let
    choiceConfig a =
      { label: cyclePeriodChoiceToLabel a
      , helpText: DOOM.text <$> helpText
      , disabled: false
      }
  unFormBuilder $ choiceField'
    (UseSelectField choiceConfig)
    Nothing
    props

cycleStubChoiceToLabel :: A.Stub -> String
cycleStubChoiceToLabel = case _ of
  A.ShortStub -> "Short last stub"
  A.LongStub -> "Long last stub"

-- cycleStubField :: forall m. Monad m => _ -> FormBuilder' m A.Stub
cycleStubField props = do
  let
    choiceConfig a =
      { label: cycleStubChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    Nothing
    props

type Result = ContractTerms

cycleField label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    fieldsFormBuilder fieldErrorId = ado
      n <- do
        let
          choices = SelectFieldChoices $ ArrayAL.solo' 1 (2 .. 31) <#> \i -> selectFieldChoice (show i) (show i)
          choiceConfig =
            { choices
            , validator: requiredV' $ Batteries.stringifyValidator Batteries.Int.validator
            , inline: true
            , initial: "1"
            }
        choiceField choiceConfig

      p <- cyclePeriodField { inline: true }
      stub <- cycleStubField { inline: true, helpText: DOOM.text <$> helpText }
      in
        { n, p, stub, includeEndDay: false }

  unFormBuilder $ multiField label (DOOM.text <$> helpText) fieldsFormBuilder

getActusDescription fieldName actusTerms = do
  fieldPropsJson <- Foreign.Object.lookup fieldName actusTerms
  fieldPropsObj <- A.toObject fieldPropsJson
  descriptionJson <- Foreign.Object.lookup "description" fieldPropsObj
  A.toString descriptionJson

scheduleConfigField = formBuilderT do
  calendarHelpText <- lift $ asks $ _.terms >>> getActusDescription "calendar"
  businessDayConventionHelpText <- lift $ asks $ _.terms >>> getActusDescription "businessDayConvention"
  endOfMonthConventionHelpText <- lift $ asks $ _.terms >>> getActusDescription "endOfMonthConvention"

  let
    calendarField :: forall m. Monad m => FormBuilderT' m Effect A.Calendar
    calendarField = do
      let
        choiceConfig a =
          { label: calendarChoiceToLabel a
          , helpText: DOOM.text <$> calendarHelpText
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { inline: true
        , helpText: DOOM.text <$> calendarHelpText
        }

    businessDayConventionField :: forall m. Monad m => FormBuilderT' m Effect A.BDC
    businessDayConventionField = do
      let
        choiceConfig a =
          { label: businessDayConventionChoiceToLabel a
          , helpText: Nothing
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { inline: true
        , helpText: DOOM.text <$> businessDayConventionHelpText
        }

    -- { label: Just $ DOOM.text "Business day convention" }

    endOfMonthConventionField :: forall m. Monad m => FormBuilderT' m Effect A.EOMC
    endOfMonthConventionField = do
      let
        choiceConfig a =
          { label: endOfMonthConventionChoiceToLabel a
          , helpText: Nothing
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { inline: true
        , helpText: endOfMonthConventionHelpText <#> DOOM.text
        }
  -- { label: Just $ DOOM.text "End of month convention" }

  let
    fieldsFormBuilder fieldErrorId = ado
      calendar <- calendarField
      endOfMonthConvention <- endOfMonthConventionField
      businessDayConvention <- businessDayConventionField
      in
        { calendar
        , businessDayConvention
        , endOfMonthConvention
        }
  unFormBuilder $ multiField (Just $ DOOM.text "Schedule config") Nothing $ fieldsFormBuilder

reqDateTimeField label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    helpText' = DOOM.text <$> helpText
  unFormBuilder $ dateTimeField (Just label) helpText' reqValidator'

pseudoReqDateTimeField label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    helpText' = DOOM.text <$> helpText
  unFormBuilder $ dateTimeField (Just label) helpText' pseudoReqValidator'

optDateTimeField label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    helpText' = DOOM.text <$> helpText
  unFormBuilder $ dateTimeField (Just label) helpText' identity

-- decimalInput' label actusFieldName = formBuilderT do
--   helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
--   let
--     helpText' = DOOM.text <$> helpText
--   unFormBuilder $ decimalInput { label: Just label, helpText: helpText' }

pseudoReqValidator missingError = liftFnMaybe (const [ missingError ]) case _ of
  Nothing -> Nothing
  Just a -> Just (Just a)

pseudoReqValidator' = pseudoReqValidator "This field is required"

reqValidator missingError = liftFnMaybe (const [ missingError ]) identity

reqValidator' = reqValidator "This field is required"

optDecimalInput label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    helpText' = DOOM.text <$> helpText
  unFormBuilder $ decimalInput
    { label: Just label
    , helpText: helpText'
    , validator: identity
    }

-- This is pretty funny:
-- * we create a standard decimalInput field,
-- * it is gonna output a `Maybe Decimal`.
-- * we want to actually run additional validator which checks if the field is not empty
--   but at the the end we want to return a `Maybe Decimal` again.
pseudoReqDecimalInput label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    helpText' = DOOM.text <$> helpText
  unFormBuilder $ decimalInput
    { label: Just label
    , helpText: helpText'
    , validator: pseudoReqValidator'
    }

reqDecimalInput label actusFieldName = formBuilderT do
  helpText <- lift $ asks $ _.terms >>> getActusDescription actusFieldName
  let
    helpText' = DOOM.text <$> helpText
  unFormBuilder $ decimalInput
    { label: Just label
    , helpText: helpText'
    , validator: reqValidator'
    }

data Optionality = Required | Optional
derive instance Generic Optionality _
instance Show Optionality where
  show Required = "Required"
  show Optional = "Optional"

mkAmortizingLoanForm :: AmortizingLoanChoice -> ReaderT ActusDictionaries Effect (BootstrapForm Effect Query Result)
mkAmortizingLoanForm amortizingLoanChoice = do
  (applicabilityRulesDictJson :: Object Json) <- asks _.applicability
  let
    actusLabel = amortizingLoanActusDictLabel amortizingLoanChoice
    fieldsOptionality = do
      let
        contractApplicabilityRules :: Object Json
        contractApplicabilityRules = fromMaybe Object.empty do
          -- applicabilityRulesJson <- "applicability" `Object.lookup` applicabilityRulesDictJson
          --applicabilityRules <- Argonaut.toObject applicabilityRulesDictJson
          actusLabel `Object.lookup` applicabilityRulesDictJson >>= Argonaut.toObject

      Map.catMaybes $ Map.fromFoldableWithIndex $ mapWithIndexFlipped contractApplicabilityRules \fieldName fieldJson -> do
        Alternative.guard (not $ fieldName `Array.elem` ["contract", "contractType"]) *> do
          -- We use the same strategy like in the official demo
          -- to derive optinality of a field:
          -- https://github.com/actusfrf/actus-webapp/blob/0c426e9ff6fce9a45be4b1255e801bf10abc91ad/frontend/src/components/Form/index.js#L577
          fieldStr <- Argonaut.toString fieldJson
          if fieldStr == "NN" then Just Required
          else Just Optional

    knownFields =
      [ "cycleOfInterestPayment"
      , "initialExchangeDate"
      , "cycleAnchorDateOfInterestPayment"
      , "statusDate"
      , "maturityDate"
      , "nominalInterestRate"
      , "notionalPrincipal"
      , "premiumDiscountAtIED"
      , "rateMultiplier"
      ]

    mkOptionalField
      :: forall a builderM validatorM
       . Monad builderM
      => Monad validatorM
      => String
      -> FormBuilderT builderM validatorM Query (Maybe a)
      -> FormBuilderT builderM validatorM Query (Maybe a)
      -> FormBuilderT builderM validatorM Query (Maybe a)
    mkOptionalField actusFieldName optField reqField = case Map.lookup actusFieldName fieldsOptionality of
      Just Required -> reqField
      Just Optional -> optField
      Nothing -> liftBuilderM $ pure $ UseForm.liftValidator $ pure Nothing

    mkOptionalDateTimeField label actusFieldName = do
      let
        optField = optDateTimeField label actusFieldName
        reqField = pseudoReqDateTimeField label actusFieldName
      mkOptionalField actusFieldName optField reqField

    mkOptionalDecimalInput label actusFieldName = do
      let
        optField = optDecimalInput label actusFieldName
        reqField = pseudoReqDecimalInput label actusFieldName
      mkOptionalField actusFieldName optField reqField


  traceM "FORM?"
  forWithIndex_ fieldsOptionality \fieldName fieldOptionality -> do
    if (fieldName `Array.elem` knownFields)
      then traceM $ "Known field: " <> fieldName <> " with optionality: " <> show fieldOptionality
      else traceM $ "Unknown field: " <> fieldName <> " with optionality: " <> show fieldOptionality

  FormBuilder.evalBuilderT' ado
    contractId <- do
      let
        props =
          { initial: ""
          , label: Just $ DOOM.text "Contract ID"
          , validator: requiredV' $ identity
          , helpText: Nothing
          }
      FormBuilder.textInput props
    { businessDayConvention, calendar, endOfMonthConvention } <- scheduleConfigField

    dayCountConvention <- dayCountConventionField
    contractRole <- contractRoleField

    let
      currency = "DjedTestUSD"

    -- initialExchangeDate = "2024-01-01T00:00:00", """
    cycleOfInterestPayment <- cycleField (Just $ DOOM.text "Cycle of interest payment") "cycleOfInterestPayment"

    initialExchangeDate <- mkOptionalDateTimeField (DOOM.text "Initial exchange date") "initialExchangeDate"

    cycleAnchorDateOfInterestPayment <- mkOptionalDateTimeField (DOOM.text "Cycle anchor date of interest payment") "cycleAnchorDateOfInterestPayment"

    statusDate <- reqDateTimeField (DOOM.text "Status date") "statusDate"

    maturityDate <- mkOptionalDateTimeField (DOOM.text "Maturity date") "maturityDate"

    nominalInterestRate <- mkOptionalDecimalInput (DOOM.text "Nominal interest rate") "nominalInterestRate"

    notionalPrincipal <- mkOptionalDecimalInput (DOOM.text "Notional principal") "notionalPrincipal"

    premiumDiscountAtIED <- mkOptionalDecimalInput (DOOM.text "Premium discount at IED") "premiumDiscountAtIED"

    rateMultiplier <- mkOptionalDecimalInput (DOOM.text "Rate multiplier") "rateMultiplier"

    in
      mkContractTerms
        { contractType: amortizingLoanToContractType amortizingLoanChoice
        , contractId
        , dayCountConvention: Just dayCountConvention
        , statusDate
        , scheduleConfig:
            { calendar: Just calendar
            , businessDayConvention: Just businessDayConvention
            , endOfMonthConvention: Just endOfMonthConvention
            }
        , contractRole
        , cycleOfInterestPayment: Just cycleOfInterestPayment
        , initialExchangeDate
        , cycleAnchorDateOfInterestPayment
        , maturityDate: maturityDate
        , nominalInterestRate: nominalInterestRate
        , notionalPrincipal: notionalPrincipal
        , premiumDiscountAtIED: premiumDiscountAtIED
        , rateMultiplier: rateMultiplier
        }

mkAmortizingLoanForms = do
  annuity <- mkAmortizingLoanForm Annuity
  principalAtMaturity <- mkAmortizingLoanForm PrincipalAtMaturity
  negativeAmortizer <- mkAmortizingLoanForm NegativeAmortizer
  linearAmortizer <- mkAmortizingLoanForm LinearAmortizer
  pure
    { annuity
    , principalAtMaturity
    , negativeAmortizer
    , linearAmortizer
    }

mkJsonForm :: _ -> BootstrapForm Effect Query Result
mkJsonForm cardanoMultiplatformLib = FormBuilder.evalBuilder' $ FormBuilder.textArea
  { missingError: "Please provide contract terms JSON value"
  , helpText: Just $ DOOM.div_
      [ DOOM.text "We gonna perform only a basic JSON validation in here and we won't perform any ACTUS applicablity checks."
      , DOOM.br {}
      , DOOM.text "We implemented a more robust validation schemes in the case of the any other create contract flow than this one."
      ]
  , initial: initialJson
  , validator: requiredV' $ Validator.liftFnEither \jsonString -> do
      json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
      lmap (Array.singleton <<< show) (decodeJson json)
  , rows: 15
  , name: (Just $ FieldId "contract-terms")
  }

type Props =
  { contractFormTypeChoice :: ContractFormTypeChoice
  , onSuccess :: Result -> Effect Unit
  -- , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  actusDictionaries <- asks _.actusDictionaries

  amortizingLoanForms <- liftEffect $ runReaderT mkAmortizingLoanForms actusDictionaries

  let
    applicability = actusDictionaries.applicability

    rules =
      { annuity: applicability `flip Foreign.Object.lookup` amortizingLoanActusDictLabel Annuity >>= A.toObject
      , linearAmortizer: applicability `flip Foreign.Object.lookup` amortizingLoanActusDictLabel LinearAmortizer >>= A.toObject
      , negativeAmortizer: applicability `flip Foreign.Object.lookup` amortizingLoanActusDictLabel NegativeAmortizer >>= A.toObject
      , principalAtMaturity: applicability `flip Foreign.Object.lookup` amortizingLoanActusDictLabel PrincipalAtMaturity >>= A.toObject
      }

  for_ rules.annuity \annuity ->
    forWithIndex_ annuity \key rule -> do
      let
        compare otherRules = do
          let
            rule' = do
              otherRules' <- otherRules
              key `Foreign.Object.lookup` otherRules'
          when (Just rule /= rule') do
            case rule' of
              Just rule'' -> do
                traceM key
                traceM rule
                traceM rule''
              Nothing -> do
                traceM "missing key:"
                traceM key
      compare rules.linearAmortizer
      compare rules.negativeAmortizer
      compare rules.principalAtMaturity

  liftEffect $ component "CreateContract.SecondStep" \{ contractFormTypeChoice, onSuccess, onDismiss, inModal } -> React.do
    let
      onSubmit = _.result >>> case _ of
        Just (V (Right contractFormType) /\ _) -> do
          onSuccess contractFormType
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit
      form = case contractFormTypeChoice of
        AmortizingLoans amortizingLoan -> case amortizingLoan of
          Annuity -> amortizingLoanForms.annuity
          LinearAmortizer -> amortizingLoanForms.linearAmortizer
          NegativeAmortizer -> amortizingLoanForms.negativeAmortizer
          PrincipalAtMaturity -> amortizingLoanForms.principalAtMaturity
        _ -> mkJsonForm cardanoMultiplatformLib

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    pure $ do
      let
        fields = UseForm.renderForm form formState
        formBody = DOM.div { className: "form-group" } fields
        formActions = DOOM.fragment
          [ link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          , DOM.button
              do
                let
                  disabled = case result of
                    Just (V (Right _) /\ _) -> false
                    _ -> true
                { className: "btn btn-primary"
                , onClick: onSubmit'
                , disabled
                }
              [ R.text "Submit" ]
          ]

      if inModal then modal
        { title: R.text "Add contract | Step 2 of 4"
        , onDismiss
        , body: DOM.div { className: "row" }
            [ DOM.div { className: "col-12" } [ formBody ]
            -- , DOM.div { className: "col-3" } [ DOOM.text "TEST" ]
            ]
        , footer: formActions
        , size: Modal.ExtraLarge
        }
      else
        formBody

initialJson :: String
initialJson = String.joinWith "\n"
  [ "{"
  , """ "contractType": "PAM", """
  , """ "contractID": "pam01", """
  , """ "statusDate": "2023-12-31T00:00:00", """
  , """ "contractDealDate": "2023-12-28T00:00:00", """
  , """ "currency": "ADA", """
  , """ "notionalPrincipal": "20", """
  , """ "initialExchangeDate": "2024-01-01T00:00:00", """
  , """ "maturityDate": "2025-01-01T00:00:00", """
  , """ "nominalInterestRate": "0.1", """
  , """ "cycleAnchorDateOfInterestPayment": "2025-01-01T00:00:00", """
  , """ "cycleOfInterestPayment": "P1YL0", """
  , """ "dayCountConvention": "30E360", """
  , """ "endOfMonthConvention": "SD", """
  , """ "premiumDiscountAtIED": "   0", """
  , """ "rateMultiplier": "1.0", """
  , """ "contractRole": "RPA" """
  , "}"
  ]
