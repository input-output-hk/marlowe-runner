module Component.ContractTemplates.Escrow where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component)

type Props =
  { onSuccess :: V1.Contract -> Effect Unit
  , onDismiss :: Effect Unit
  }

content :: String
content = """
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
        (link
            { label: DOOM.text "Cancel"
            , onClick: onDismiss
            , showBorders: true
            }
        )
      }

