module Component.ContractTemplates.Swap where

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
     (Deposit (Role "Ada provider") (Role "Ada provider")
        (Token "" "")
        (MulValue
           (Constant 1000000)
           (ConstantParam "Amount of Ada")))
     (When [
        (Case
           (Deposit (Role "Dollar provider") (Role "Dollar provider")
              (Token "85bb65" "dollar")
              (ConstantParam "Amount of dollars"))
           (Pay (Role "Ada provider")
              (Party (Role "Dollar provider"))
              (Token "" "")
              (MulValue
                 (Constant 1000000)
                 (ConstantParam "Amount of Ada"))
              (Pay (Role "Dollar provider")
                 (Party (Role "Ada provider"))
                 (Token "85bb65" "dollar")
                 (ConstantParam "Amount of dollars") Close)))] (TimeParam "Timeout for dollar deposit") Close))] (TimeParam "Timeout for Ada deposit") Close
"""

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractTemplates.Swap" \{ onSuccess, onDismiss } -> React.do
    pure $ BodyLayout.component
      { title: "Swap"
      , description: DOOM.text "Takes Ada from one party and dollar tokens from another party, and it swaps them atomically."
      , content: wrappedContentWithFooter
          (DOM.pre {} content)
          ( link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          )
      }

