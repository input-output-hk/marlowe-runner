module Component.ContractTemplates.Escrow where

import Prelude

import CardanoMultiplatformLib (Bech32)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Data.BigInt.Argonaut (BigInt)
import Component.Widgets.Form (addressInput)
import Contrib.Polyform.FormSpecBuilder (FormSpecBuilderT, formSpecBuilderT)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatelessFormSpec as StatelessFormSpec
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (FieldLayout(..), StatelessBootstrapFormSpec, booleanField, dateTimeField, intInput, multiField)
import Control.Monad.Reader (asks)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (Query)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Polyform.Validator (liftFnMaybe)
import Polyform.Validator (liftFnM, liftFnMaybe)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, fragment, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)

-- import React.Basic.Hooks.UseForm (Form(..), liftValidator, useForm)
-- import React.Basic.Hooks.UseForm as UseForm
-- import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (StatlessBootstrapFormSpec)

type Props =
  { onSuccess :: V1.Contract -> Effect Unit
  , onDismiss :: Effect Unit
  }

mkEscrowContract
  :: { complaintDeadline :: Instant
     , complaintResponseDeadline :: Instant
     , mediationDeadline :: Instant
     , paymentDeadline :: Instant
     , price :: BigInt
     }
  -> V1.Contract
mkEscrowContract { price, mediationDeadline, paymentDeadline, complaintDeadline, complaintResponseDeadline } =
  V1.When
    [ ( V1.Case
          ( V1.Deposit (V1.Role "Seller") (V1.Role "Buyer")
              (V1.Token "" "")
              (V1.Constant price)
          )
          ( V1.When
              [ ( V1.Case
                    ( V1.Choice
                        (V1.ChoiceId "Everything is alright" (V1.Role "Buyer"))
                        [ (V1.Bound zero zero)
                        ]
                    )
                    V1.Close
                )
              , ( V1.Case
                    ( V1.Choice
                        (V1.ChoiceId "Report problem" (V1.Role "Buyer"))
                        [ (V1.Bound one one)
                        ]
                    )
                    ( V1.Pay (V1.Role "Seller")
                        (V1.Account (V1.Role "Buyer"))
                        (V1.Token "" "")
                        (V1.Constant price)
                        ( V1.When
                            [ ( V1.Case
                                  ( V1.Choice
                                      (V1.ChoiceId "Confirm problem" (V1.Role "Seller"))
                                      [ (V1.Bound one one)
                                      ]
                                  )
                                  V1.Close
                              )
                            , ( V1.Case
                                  ( V1.Choice
                                      (V1.ChoiceId "Dispute problem" (V1.Role "Seller"))
                                      [ (V1.Bound zero zero)
                                      ]
                                  )
                                  ( V1.When
                                      [ ( V1.Case
                                            ( V1.Choice
                                                (V1.ChoiceId "Dismiss claim" (V1.Role "Mediator"))
                                                [ (V1.Bound zero zero)
                                                ]
                                            )
                                            ( V1.Pay (V1.Role "Buyer")
                                                (V1.Party (V1.Role "Seller"))
                                                (V1.Token "" "")
                                                (V1.Constant price)
                                                V1.Close
                                            )
                                        )
                                      , ( V1.Case
                                            ( V1.Choice
                                                (V1.ChoiceId "Confirm problem" (V1.Role "Mediator"))
                                                [ (V1.Bound one one)
                                                ]
                                            )
                                            V1.Close
                                        )
                                      ]
                                      mediationDeadline
                                      V1.Close
                                  )
                              )
                            ]
                            complaintResponseDeadline
                            V1.Close
                        )
                    )
                )
              ]
              complaintDeadline
              V1.Close
          )
      )
    ]
    paymentDeadline
    V1.Close

reqValidator missingError = liftFnMaybe (const [ missingError ]) identity

reqValidator' = reqValidator "This field is required"

data RoleTokenUsage = UseRoleTokens | UseAddress String

roleTokenUsageField
  :: forall builderM validatorM
   . Monad builderM
  => MonadEffect validatorM
  => CardanoMultiplatformLib.Lib
  -> Maybe JSX
  -> Maybe JSX
  -> FormSpecBuilderT builderM (StatelessBootstrapFormSpec validatorM) Query _ -- (Maybe Bech32)
roleTokenUsageField cardanoMultiplatformLib possibleLabel possibleHelpText = do
  let
    addrFieldId = FieldId "addr"

    fieldsFormBuilder
      :: FieldId
      -> FormSpecBuilderT builderM (StatelessBootstrapFormSpec validatorM) Query { useRoleTokens :: Boolean, possibleAddress :: Maybe Bech32 }
    fieldsFormBuilder _ = do
      let
        setUseRoleToken = formSpecBuilderT $ pure $ StatelessFormSpec.liftValidator $ liftFnM \useRoleTokens -> do
          -- put useRoleTokens
          pure useRoleTokens

      ado
        useRoleTokens <- setUseRoleToken <<< booleanField
          { layout: Inline
          , initial: true
          }

        possibleAddress <- addressInput
          cardanoMultiplatformLib
          { layout: Inline
          , name: Just addrFieldId
          }
        -- { layout: FormBuilder.Inline
        -- , validator: identity
        -- , name: addrFieldId
        -- }
        in
          { useRoleTokens, possibleAddress }

  --roleTokenUsage = liftValidator $ liftFnEither \{ useRoleTokens, possibleAddress } -> case useRoleTokens, possibleAddress of
  --  true, _ -> pure UseRoleTokens
  --  false, Just address -> pure $ UseAddress address
  --  false, Nothing  -> Left $ Map.singleton addrFieldId [ "Address is required when you don't want to use role tokens" ]

  multiField possibleLabel possibleHelpText fieldsFormBuilder

mkEscrowForm
  :: CardanoMultiplatformLib.Lib
  -> StatelessBootstrapFormSpec
       Effect
       Query
       { mediationDeadline :: Instant
       , price :: BigInt
       , complaintDeadline :: Instant
       , complaintResponseDeadline :: Instant
       , paymentDeadline :: Instant
       }
mkEscrowForm cardanoMultiplatformLib = FormSpecBuilder.evalBuilder Nothing $ ado
  price <- intInput
    { helpText: Nothing -- Just $ DOOM.text "Price"
    , initial: ""
    , label: Just $ DOOM.text "Price"
    , touched: false
    }
  mediationDeadline <- dateTimeField (Just $ DOOM.text "Mediation timeout") (Just $ DOOM.text "MEDIATOR timeout help") reqValidator'
  complaintDeadline <- dateTimeField (Just $ DOOM.text "Complaint Deadline timeout") (Just $ DOOM.text "COMPLAINT timeout help") reqValidator'
  complaintResponseDeadline <- dateTimeField (Just $ DOOM.text "Complaint Response Deadline timeout") (Just $ DOOM.text "COMPLAINT RESPONSE timeout help") reqValidator'
  paymentDeadline <- dateTimeField (Just $ DOOM.text "Payment Deadline timeout") (Just $ DOOM.text "PAYMENT timeout help") reqValidator'

  -- mediatorParty <- roleTokenUsageField cardanoMultiplatformLib (Just $ DOOM.text "Mediator") (Just $ DOOM.text "Use role token")
  -- buyerParty <- roleTokenUsageField cardanoMultiplatformLib (Just $ DOOM.text "Buyer") (Just $ DOOM.text "Use role token")
  -- sellerParty <- roleTokenUsageField cardanoMultiplatformLib (Just $ DOOM.text "Seller") (Just $ DOOM.text "Use role token")
  in
    { price: BigInt.fromInt price
    , mediationDeadline: Instant.fromDateTime mediationDeadline
    , complaintDeadline: Instant.fromDateTime complaintDeadline
    , complaintResponseDeadline: Instant.fromDateTime complaintResponseDeadline
    , paymentDeadline: Instant.fromDateTime paymentDeadline
    }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  let
    formSpec = mkEscrowForm cardanoMultiplatformLib

  liftEffect $ component "ContractTemplates.Escrow" \{ onSuccess, onDismiss } -> React.do

    possibleContract /\ setContract <- React.useState' Nothing

    let
      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right escrowParams) /\ _) -> setContract $ Just $ mkEscrowContract escrowParams
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: formSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      -- , state: false
      }

    let
      fields = StatelessFormSpec.renderFormSpec formSpec formState
      formBody = case possibleContract of
        Nothing -> DOM.div { className: "form-group" } fields
        Just contract -> marloweYaml contract
      formActions = fragment
        [ DOM.div { className: "row" } $
            [ DOM.div { className: "col-6 text-start" } $
                [ link
                    { label: DOOM.text "Cancel"
                    , onClick: onDismiss
                    , showBorders: true
                    , extraClassNames: "me-3"
                    }
                ]
            , DOM.div { className: "col-6 text-end" } $
                [ DOM.button
                    do
                      let
                        disabled = case result of
                          Just (V (Right _) /\ _) -> false
                          _ -> true
                      { className: "btn btn-primary"
                      , onClick: onSubmit'
                      , disabled
                      }
                    [ DOOM.text "Submit" ]
                ]
            ]
        ]
    pure $ BodyLayout.component
      { title: "Escrow"
      , description: DOOM.text "Regulates a money exchange between a \"Buyer\" and a \"Seller\". If there is a disagreement, an \"Mediator\" will decide whether the money is refunded or paid to the \"Seller\"."
      , content: wrappedContentWithFooter
          formBody
          formActions
      }
