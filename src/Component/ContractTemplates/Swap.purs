module Component.ContractTemplates.Swap where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Contrib.Polyform.FormSpecBuilder (evalBuilder')
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (StatelessBootstrapFormSpec, dateTimeField, intInput, textInput)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (Query)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Polyform.Validator (liftFnMaybe)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX, component, fragment, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.UseStatelessFormSpec (useStatelessFormSpec)

type Props =
  { onSuccess :: V1.Contract -> Effect Unit
  , onDismiss :: Effect Unit
  }

mkSwapContract
  :: { tokenADepositDeadline :: Instant
     , tokenBDepositDeadline :: Instant
     , tokenACurrencySymbol :: String
     , tokenBCurrencySymbol :: String
     , tokenAName :: String
     , tokenBName :: String
     , tokenAAmount :: BigInt
     , tokenBAmount :: BigInt
     }
  -> V1.Contract
mkSwapContract { tokenADepositDeadline, tokenBDepositDeadline, tokenACurrencySymbol, tokenBCurrencySymbol, tokenAAmount, tokenBAmount, tokenAName, tokenBName } = do
  -- NOTES: Possible Validation Rules
  -- 1. Token A and Token B must be different
  -- 2. Should we check if ada is being used and if so, multiply by 1,000,000 to get lovelace amount
  -- 3. Check Ada amount is not greater than total supply
  -- 4. Check against deadlines i.e. deposit should be sooner than swap
  V1.When
    [ ( V1.Case
          ( V1.Deposit (V1.Role "Token A provider") (V1.Role "Token A provider")
              (V1.Token tokenACurrencySymbol tokenAName)
              (V1.Constant tokenAAmount)
          )
          ( V1.When
              [ ( V1.Case
                    ( V1.Deposit (V1.Role "Token B provider") (V1.Role "Token B provider")
                        (V1.Token tokenBCurrencySymbol tokenBName)
                        (V1.Constant tokenBAmount)
                    )
                    ( V1.Pay (V1.Role "Token A provider")
                        (V1.Party (V1.Role "Token B provider"))
                        (V1.Token tokenACurrencySymbol tokenAName)
                        (V1.Constant tokenAAmount)
                        ( V1.Pay (V1.Role "Token B provider")
                            (V1.Party (V1.Role "Token A provider"))
                            (V1.Token tokenBCurrencySymbol tokenBName)
                            (V1.Constant tokenBAmount)
                            V1.Close
                        )
                    )
                )
              ]
              tokenBDepositDeadline
              V1.Close
          )
      )
    ]
    tokenADepositDeadline
    V1.Close

reqValidator missingError = liftFnMaybe (const [ missingError ]) identity

reqValidator' = reqValidator "This field is required"

swapFormSpec
  :: StatelessBootstrapFormSpec Effect Query
       { tokenAAmount :: BigInt
       , tokenBAmount :: BigInt
       , tokenAName :: String
       , tokenBName :: String
       , tokenACurrencySymbol :: String
       , tokenBCurrencySymbol :: String
       , tokenADepositDeadline :: Instant
       , tokenBDepositDeadline :: Instant
       }
swapFormSpec = evalBuilder' $ ado
  tokenAAmount <- intInput
    { helpText: Nothing
    , initial: ""
    , label: Just $ DOOM.text "Token A Amount"
    , touched: false
    }
  tokenAName <- textInput
    { helpText: Nothing
    , initial: ""
    , label: Just $ DOOM.text "Token A Name"
    , touched: false
    , validator: reqValidator'
    }
  tokenACurrencySymbol <- textInput
    { helpText: Nothing
    , initial: ""
    , label: Just $ DOOM.text "Token A Currency Symbol"
    , touched: false
    , validator: reqValidator'
    }
  tokenBAmount <- intInput
    { helpText: Nothing
    , initial: ""
    , label: Just $ DOOM.text "Token B Amount"
    , touched: false
    }
  tokenBName <- textInput
    { helpText: Nothing
    , initial: ""
    , label: Just $ DOOM.text "Token B Name"
    , touched: false
    , validator: reqValidator'
    }
  tokenBCurrencySymbol <- textInput
    { helpText: Nothing
    , initial: ""
    , label: Just $ DOOM.text "Token B Currency Symbol"
    , touched: false
    , validator: reqValidator'
    }

  tokenADepositDeadline <- dateTimeField (Just $ DOOM.text "Token A Deposit timeout") (Just $ DOOM.text "Token A timeout help") reqValidator'
  tokenBDepositDeadline <- dateTimeField (Just $ DOOM.text "Token B Deposit timeout") (Just $ DOOM.text "Token B timeout help") reqValidator'
  in
    { tokenAAmount: BigInt.fromInt tokenAAmount
    , tokenAName: tokenAName
    , tokenACurrencySymbol: tokenACurrencySymbol
    , tokenBAmount: BigInt.fromInt tokenBAmount
    , tokenBName: tokenBName
    , tokenBCurrencySymbol: tokenBCurrencySymbol
    , tokenADepositDeadline: Instant.fromDateTime tokenADepositDeadline
    , tokenBDepositDeadline: Instant.fromDateTime tokenBDepositDeadline
    }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractTemplates.Swap" \{ onSuccess, onDismiss } -> React.do

    possibleContract /\ setContract <- React.useState' Nothing
    let
      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right swapParams) /\ _) -> setContract $ Just $ mkSwapContract swapParams
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: swapFormSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    let
      fields = renderFormSpec swapFormSpec formState
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
      { title: "Swap"
      , description: DOOM.text "Takes currency amount from one party and another currency amount from another party, and it swaps them atomically."
      , content: wrappedContentWithFooter
          formBody
          formActions
      }
