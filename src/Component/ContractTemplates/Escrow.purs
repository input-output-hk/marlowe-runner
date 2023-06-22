module Component.ContractTemplates.Escrow where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Data.BigInt.Argonaut (BigInt(..))
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime(..))
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.FormURLEncoded.Query (Query(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Polyform.Validator (liftFn, liftFnMaybe)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component)
import ReactBootstrap.FormBuilder (BootstrapForm)
import ReactBootstrap.FormBuilder as FormBuilder

type Props =
  { onSuccess :: V1.Contract -> Effect Unit
  , onDismiss :: Effect Unit
  }

-- mkContractForm :: (Maybe V1.Contract /\ AutoRun) -> BootstrapForm Effect Query Result
-- mkContractForm (possibleInitialContract /\ (AutoRun initialAutoRun)) = FormBuilder.evalBuilder' $ ado
--   contract <- FormBuilder.textArea
--     { missingError: "Please provide contract terms JSON value"
--     , helpText: Just $ DOOM.div_
--         [ DOOM.text "Basic JSON validation"
--         ]
--     , initial: case possibleInitialContract of
--         Nothing -> ""
--         Just initialContract -> stringifyWithIndent 2 $ encodeJson initialContract
--     , label: Just $ DOOM.text "Contract JSON"
--     , touched: isJust possibleInitialContract
--     , validator: requiredV' $ Validator.liftFnEither \jsonString -> do
--         json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
--         lmap (Array.singleton <<< show) (decodeJson json)
--     , rows: 15
--     , name: Just contractFieldId
--     }
-- 
--   tags <- FormBuilder.textInput
--     { helpText: Just $ DOOM.div_
--         [ DOOM.text "Tags"
--         ]
--     , initial: ""
--     , label: Just $ DOOM.text "Tags"
--     , touched: false
--     , validator: liftFn case _ of
--         Nothing -> Tags mempty
--         Just tags ->
--           (Tags $ Map.singleton runLiteTag
--              (Metadata $ Map.fromFoldableWithIndex
--                $ map (encodeJson <<< trim) $ split (Pattern ",") tags))
--     }
-- 
--   autoRun <- AutoRun <$> do
--     -- FIXME: This should be documented I left this as an example of more hard core lifting of validator
--     -- let
--     --   toAutoRun = liftBuilderM $ pure $ liftValidator $ liftFnM \value -> do
--     --       let
--     --         value' = AutoRun value
--     --       -- onAutoRunChange value'
--     --       pure value'
--     FormBuilder.booleanField
--       { label: DOOM.text "Auto run"
--       , helpText: DOOM.text "Whether to run the contract creation process automatically"
--       , initial: initialAutoRun
--       }
--   in
--     contract /\ tags /\ autoRun

mkEscrowContract ::
  { complaintDeadline :: Instant
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

mkEscrowForm :: BootstrapForm Effect Query { mediatorTimeout :: Instant, price :: BigInt }
mkEscrowForm = FormBuilder.evalBuilder' $ ado
  price <- FormBuilder.intInput
    { helpText: Nothing -- Just $ DOOM.text "Price"
    , initial: ""
    , label: Just $ DOOM.text "Price"
    , touched: false
    }
  mediatorTimeout <- FormBuilder.dateTimeField (Just $ DOOM.text "Mediator timeout") (Just $ DOOM.text "MEDIATOR timoeut help") reqValidator'
  in
    { price: BigInt.fromInt price
    , mediatorTimeout: Instant.fromDateTime mediatorTimeout
    }

content :: String
content =
  """
When [
  (Case
     (Deposit (Role "Seller") (Role "Buyer")
        (Token "" "")
        (ConstantParam "Price"))
     (When [
           (Case
              (Choice
                 (ChoiceId "Everything is alright" (Role "Buyer")) [
                 (Bound 0 0)]) Close)
           ,
           (Case
              (Choice
                 (ChoiceId "Report problem" (Role "Buyer")) [
                 (Bound 1 1)])
              (Pay (Role "Seller")
                 (Account (Role "Buyer"))
                 (Token "" "")
                 (ConstantParam "Price")
                 (When [
                       (Case
                          (Choice
                             (ChoiceId "Confirm problem" (Role "Seller")) [
                             (Bound 1 1)]) Close)
                       ,
                       (Case
                          (Choice
                             (ChoiceId "Dispute problem" (Role "Seller")) [
                             (Bound 0 0)])
                          (When [
                                (Case
                                   (Choice
                                      (ChoiceId "Dismiss claim" (Role "Mediator")) [
                                      (Bound 0 0)])
                                   (Pay (Role "Buyer")
                                      (Party (Role "Seller"))
                                      (Token "" "")
                                      (ConstantParam "Price") Close))
                                ,
                                (Case
                                   (Choice
                                      (ChoiceId "Confirm problem" (Role "Mediator")) [
                                      (Bound 1 1)]) Close)] (TimeParam "Mediation deadline") Close))] (TimeParam "Complaint response deadline") Close)))] (TimeParam "Complaint deadline") Close))] (TimeParam "Payment deadline") Close
"""

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractTemplates.Escrow" \{ onSuccess, onDismiss } -> React.do
    pure $ BodyLayout.component
      { title: "Escrow"
      , description: DOOM.text "Regulates a money exchange between a \"Buyer\" and a \"Seller\". If there is a disagreement, an \"Mediator\" will decide whether the money is refunded or paid to the \"Seller\"."
      , content: wrappedContentWithFooter
          (DOM.pre {} content)
          ( link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          )
      }
