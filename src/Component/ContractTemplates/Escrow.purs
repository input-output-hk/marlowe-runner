module Component.ContractTemplates.Escrow where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Data.BigInt.Argonaut (BigInt(..))
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime(..))
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (Query(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Polyform.Validator (liftFn, liftFnMaybe)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, fragment, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseForm (useForm)
import React.Basic.Hooks.UseForm as UseForm
import ReactBootstrap.FormBuilder (BootstrapForm)
import ReactBootstrap.FormBuilder as FormBuilder

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

escrowForm :: BootstrapForm Effect Query { mediationDeadline :: Instant, price :: BigInt, complaintDeadline :: Instant, complaintResponseDeadline :: Instant, paymentDeadline :: Instant }
escrowForm = FormBuilder.evalBuilder' $ ado
  price <- FormBuilder.intInput
    { helpText: Nothing -- Just $ DOOM.text "Price"
    , initial: ""
    , label: Just $ DOOM.text "Price"
    , touched: false
    }
  mediationDeadline <- FormBuilder.dateTimeField (Just $ DOOM.text "Mediator timeout") (Just $ DOOM.text "MEDIATOR timoeut help") reqValidator'
  complaintDeadline <- FormBuilder.dateTimeField (Just $ DOOM.text "Complaint Deadline timeout") (Just $ DOOM.text "COMPLAINT timoeut help") reqValidator'
  complaintResponseDeadline <- FormBuilder.dateTimeField (Just $ DOOM.text "Complaint Response Deadline timeout") (Just $ DOOM.text "COMPLAINT RESPONSE timoeut help") reqValidator'
  paymentDeadline <- FormBuilder.dateTimeField (Just $ DOOM.text "Payment Deadline timeout") (Just $ DOOM.text "PAYMENT timoeut help") reqValidator'
  in
    { price: BigInt.fromInt price
    , mediationDeadline: Instant.fromDateTime mediationDeadline
    , complaintDeadline: Instant.fromDateTime complaintDeadline
    , complaintResponseDeadline: Instant.fromDateTime complaintResponseDeadline
    , paymentDeadline: Instant.fromDateTime paymentDeadline
    }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractTemplates.Escrow" \{ onSuccess, onDismiss } -> React.do

    possibleContract /\ setContract <- React.useState' Nothing
    let
      form = escrowForm

      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        -- Just (V (Right escrowParams) /\ _) -> onSuccess $ mkEscrowContract escrowParams
        Just (V (Right escrowParams) /\ _) -> setContract $ Just $ mkEscrowContract escrowParams
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    let
      fields = UseForm.renderForm form formState
      -- formBody = DOM.div { className: "form-group" } fields
      formBody = case possibleContract of
        Nothing -> DOM.div { className: "form-group" } fields
        Just contract -> marloweYaml contract
      formActions = fragment
        [ link
            { label: DOOM.text "Cancel"
            , onClick: onDismiss
            , showBorders: true
            , extraClassNames: "me-3"
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
            [ DOOM.text "Submit" ]
        ]
    pure $ BodyLayout.component
      { title: "Escrow"
      , description: DOOM.text "Regulates a money exchange between a \"Buyer\" and a \"Seller\". If there is a disagreement, an \"Mediator\" will decide whether the money is refunded or paid to the \"Seller\"."
      , content: wrappedContentWithFooter
          formBody
          formActions
      }

