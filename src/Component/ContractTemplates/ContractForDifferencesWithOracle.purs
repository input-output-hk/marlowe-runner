module Component.ContractTemplates.ContractForDifferencesWithOracle where

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
content =
  """
When [
  (Case
     (Deposit (Role "Party") (Role "Party")
        (Token "" "")
        (ConstantParam "Amount paid by party"))
     (When [
        (Case
           (Deposit (Role "Counterparty") (Role "Counterparty")
              (Token "" "")
              (ConstantParam "Amount paid by counterparty"))
           (When [] (TimeParam "First window beginning")
              (When [
                 (Case
                    (Choice
                       (ChoiceId "dir-adausd" (Role "kraken")) [
                       (Bound 0 100000000000)])
                    (When [] (TimeParam "Second window beginning")
                       (When [
                          (Case
                             (Choice
                                (ChoiceId "inv-adausd" (Role "kraken")) [
                                (Bound 0 100000000000)])
                             (Let "Price in second window"
                                (DivValue
                                   (MulValue
                                      (ConstantParam "Amount of Ada to use as asset")
                                      (MulValue
                                         (ChoiceValue
                                            (ChoiceId "dir-adausd" (Role "kraken")))
                                         (ChoiceValue
                                            (ChoiceId "inv-adausd" (Role "kraken")))))
                                   (Constant 10000000000000000))
                                (If
                                   (ValueGT
                                      (ConstantParam "Amount of Ada to use as asset")
                                      (UseValue "Price in second window"))
                                   (Let "Decrease in price"
                                      (SubValue
                                         (ConstantParam "Amount of Ada to use as asset")
                                         (UseValue "Price in second window"))
                                      (Pay (Role "Counterparty")
                                         (Account (Role "Party"))
                                         (Token "" "")
                                         (Cond
                                            (ValueLT
                                               (UseValue "Decrease in price")
                                               (ConstantParam "Amount paid by counterparty"))
                                            (UseValue "Decrease in price")
                                            (ConstantParam "Amount paid by counterparty")) Close))
                                   (If
                                      (ValueLT
                                         (ConstantParam "Amount of Ada to use as asset")
                                         (UseValue "Price in second window"))
                                      (Let "Increase in price"
                                         (SubValue
                                            (UseValue "Price in second window")
                                            (ConstantParam "Amount of Ada to use as asset"))
                                         (Pay (Role "Party")
                                            (Account (Role "Counterparty"))
                                            (Token "" "")
                                            (Cond
                                               (ValueLT
                                                  (UseValue "Increase in price")
                                                  (ConstantParam "Amount paid by party"))
                                               (UseValue "Increase in price")
                                               (ConstantParam "Amount paid by party")) Close)) Close))))] (TimeParam "Second window deadline") Close)))] (TimeParam "First window deadline") Close)))] (TimeParam "Counterparty deposit deadline") Close))] (TimeParam "Party deposit deadline") Close
"""

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractTemplates.Swap" \{ onSuccess, onDismiss } -> React.do
    pure $ BodyLayout.component
      { title: "Contract for Differences with Oracle"
      , description: DOOM.text "\"Party\" and \"Counterparty\" deposit 100 Ada and after 60 slots these assets are redistributed depending on the change in price of 100 Ada worth of dollars between the start and the end of the contract. If the price increases, the difference goes to \"Counterparty\"; if it decreases, the difference goes to \"Party\", up to a maximum of 100 Ada."
      , content: wrappedContentWithFooter
          (DOM.pre {} content)
          ( link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          )
      }

