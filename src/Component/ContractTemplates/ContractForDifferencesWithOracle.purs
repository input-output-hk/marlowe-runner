module Component.ContractTemplates.ContractForDifferencesWithOracle where

import Prelude

import Component.BodyLayout (wrappedContentWithFooter)
import Component.BodyLayout as BodyLayout
import Component.MarloweYaml (marloweYaml)
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Contrib.Polyform.FormSpecBuilder as FormSpecBuilder
import Contrib.Polyform.FormSpecs.StatelessFormSpec (renderFormSpec)
import Contrib.ReactBootstrap.FormSpecBuilders.StatelessFormSpecBuilders (StatelessBootstrapFormSpec, dateTimeField, intInput)
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
import Partial.Unsafe (unsafeCrashWith)
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

unsafeBigIntFromString :: String -> BigInt
unsafeBigIntFromString str = case BigInt.fromString str of
  Just bigInt -> bigInt
  Nothing -> unsafeCrashWith $ "unsafeBigIntFromString failed for input: " <> str

mkContractForDifferencesWithOracleContract
  :: { partyDepositDeadline :: Instant
     , counterPartyDepositDeadline :: Instant
     , firstWindowBeginning :: Instant
     , secondWindowBeginning :: Instant
     , firstWindowDeadline :: Instant
     , secondWindowDeadline :: Instant
     , partyTokenAmount :: BigInt
     , counterPartyTokenAmount :: BigInt
     , adaAmountToUseAsAsset :: BigInt
     }
  -> V1.Contract
mkContractForDifferencesWithOracleContract { partyDepositDeadline, counterPartyDepositDeadline, firstWindowBeginning, secondWindowBeginning, firstWindowDeadline, secondWindowDeadline, partyTokenAmount, counterPartyTokenAmount, adaAmountToUseAsAsset } =
  V1.When
    [ ( V1.Case
          ( V1.Deposit (V1.Role "Party") (V1.Role "Party")
              (V1.Token "" "")
              (V1.Constant partyTokenAmount)
          )
          ( V1.When
              [ ( V1.Case
                    ( V1.Deposit (V1.Role "Counterparty") (V1.Role "Counterparty")
                        (V1.Token "" "")
                        (V1.Constant counterPartyTokenAmount)
                    )
                    ( V1.When [] (firstWindowBeginning)
                        ( V1.When
                            [ ( V1.Case
                                  ( V1.Choice
                                      (V1.ChoiceId "dir-adausd" (V1.Role "kraken"))
                                      [ (V1.Bound zero (unsafeBigIntFromString "100000000000"))
                                      ]
                                  )
                                  ( V1.When [] (secondWindowBeginning)
                                      ( V1.When
                                          [ ( V1.Case
                                                ( V1.Choice
                                                    (V1.ChoiceId "inv-adausd" (V1.Role "kraken"))
                                                    [ (V1.Bound zero (unsafeBigIntFromString "100000000000"))
                                                    ]
                                                )
                                                ( V1.Let (V1.ValueId "Price in second window")
                                                    ( V1.DivValue
                                                        ( V1.MulValue
                                                            (V1.Constant adaAmountToUseAsAsset)
                                                            ( V1.MulValue
                                                                ( V1.ChoiceValue
                                                                    (V1.ChoiceId "dir-adausd" (V1.Role "kraken"))
                                                                )
                                                                ( V1.ChoiceValue
                                                                    (V1.ChoiceId "inv-adausd" (V1.Role "kraken"))
                                                                )
                                                            )
                                                        )
                                                        (V1.Constant (unsafeBigIntFromString "100000000000"))
                                                    )
                                                    ( V1.If
                                                        ( V1.ValueGT
                                                            (V1.Constant adaAmountToUseAsAsset)
                                                            (V1.UseValue (V1.ValueId "Price in second window"))
                                                        )
                                                        ( V1.Let (V1.ValueId "Decrease in price")
                                                            ( V1.SubValue
                                                                (V1.Constant adaAmountToUseAsAsset)
                                                                (V1.UseValue (V1.ValueId "Price in second window"))
                                                            )
                                                            ( V1.Pay (V1.Role "Counterparty")
                                                                (V1.Account (V1.Role "Party"))
                                                                (V1.Token "" "")
                                                                ( V1.Cond
                                                                    ( V1.ValueLT
                                                                        (V1.UseValue (V1.ValueId "Decrease in price"))
                                                                        (V1.Constant adaAmountToUseAsAsset)
                                                                    )
                                                                    (V1.UseValue (V1.ValueId "Decrease in price"))
                                                                    (V1.Constant adaAmountToUseAsAsset)
                                                                )
                                                                V1.Close
                                                            )
                                                        )
                                                        ( V1.If
                                                            ( V1.ValueLT
                                                                (V1.Constant adaAmountToUseAsAsset)
                                                                (V1.UseValue (V1.ValueId "Price in second window"))
                                                            )
                                                            ( V1.Let (V1.ValueId "Increase in price")
                                                                ( V1.SubValue
                                                                    (V1.UseValue (V1.ValueId "Price in second window"))
                                                                    (V1.Constant adaAmountToUseAsAsset)
                                                                )
                                                                ( V1.Pay (V1.Role "Party")
                                                                    (V1.Account (V1.Role "Counterparty"))
                                                                    (V1.Token "" "")
                                                                    ( V1.Cond
                                                                        ( V1.ValueLT
                                                                            (V1.UseValue (V1.ValueId "Increase in price"))
                                                                            (V1.Constant partyTokenAmount)
                                                                        )
                                                                        (V1.UseValue (V1.ValueId "Increase in price"))
                                                                        (V1.Constant partyTokenAmount)
                                                                    )
                                                                    V1.Close
                                                                )
                                                            )
                                                            V1.Close
                                                        )
                                                    )
                                                )
                                            )
                                          ]
                                          (secondWindowDeadline)
                                          V1.Close
                                      )
                                  )
                              )
                            ]
                            (firstWindowDeadline)
                            V1.Close
                        )
                    )
                )
              ]
              (counterPartyDepositDeadline)
              V1.Close
          )
      )
    ]
    (partyDepositDeadline)
    V1.Close

contractFormSpec
  :: StatelessBootstrapFormSpec Effect Query
       { partyDepositDeadline :: Instant
       , counterPartyDepositDeadline :: Instant
       , firstWindowBeginning :: Instant
       , secondWindowBeginning :: Instant
       , firstWindowDeadline :: Instant
       , secondWindowDeadline :: Instant
       , partyTokenAmount :: BigInt
       , counterPartyTokenAmount :: BigInt
       , adaAmountToUseAsAsset :: BigInt
       }
contractFormSpec = do
  let
    reqValidator missingError = liftFnMaybe (const [ missingError ]) identity
    reqValidator' = reqValidator "This field is required"

  FormSpecBuilder.evalBuilder' $ ado
    partyTokenAmount <- intInput
      { helpText: Nothing
      , initial: ""
      , label: Just $ DOOM.text "Party Token Amount"
      , touched: false
      }

    counterPartyTokenAmount <- intInput
      { helpText: Nothing
      , initial: ""
      , label: Just $ DOOM.text "Counter-Party Token Amount"
      , touched: false
      }

    adaAmountToUseAsAsset <- intInput
      { helpText: Nothing
      , initial: ""
      , label: Just $ DOOM.text "ADA Amount to use as asset"
      , touched: false
      }

    partyDepositDeadline <- dateTimeField (Just $ DOOM.text "Party Deposit timeout") (Just $ DOOM.text "Party timeout help") reqValidator'
    counterPartyDepositDeadline <- dateTimeField (Just $ DOOM.text "Counter-Party Deposit timeout") (Just $ DOOM.text "Counter-Party timeout help") reqValidator'
    firstWindowBeginning <- dateTimeField (Just $ DOOM.text "First Window Beginning timeout") (Just $ DOOM.text "First Window Beginning timeout help") reqValidator'
    firstWindowDeadline <- dateTimeField (Just $ DOOM.text "First Window Deadline timeout") (Just $ DOOM.text "First Window Deadline timeout help") reqValidator'
    secondWindowBeginning <- dateTimeField (Just $ DOOM.text "Second Window Beginning timeout") (Just $ DOOM.text "Second Window Beginning timeout help") reqValidator'
    secondWindowDeadline <- dateTimeField (Just $ DOOM.text "Second Window Ending timeout") (Just $ DOOM.text "Second Window Deadline timeout help") reqValidator'
    in
      { partyTokenAmount: BigInt.fromInt partyTokenAmount
      , counterPartyTokenAmount: BigInt.fromInt counterPartyTokenAmount
      , adaAmountToUseAsAsset: BigInt.fromInt adaAmountToUseAsAsset
      , partyDepositDeadline: Instant.fromDateTime partyDepositDeadline
      , counterPartyDepositDeadline: Instant.fromDateTime counterPartyDepositDeadline
      , firstWindowBeginning: Instant.fromDateTime firstWindowBeginning
      , firstWindowDeadline: Instant.fromDateTime firstWindowDeadline
      , secondWindowBeginning: Instant.fromDateTime secondWindowBeginning
      , secondWindowDeadline: Instant.fromDateTime secondWindowDeadline
      }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  liftEffect $ component "ContractTemplates.ContractForDifferencesWithOracle" \{ onDismiss } -> React.do

    possibleContract /\ setContract <- React.useState' Nothing
    let
      onSubmit :: _ -> Effect Unit
      onSubmit = _.result >>> case _ of
        Just (V (Right contractParams) /\ _) -> setContract $ Just $ mkContractForDifferencesWithOracleContract contractParams
        _ -> pure unit

    { formState, onSubmit: onSubmit', result } <- useStatelessFormSpec
      { spec: contractFormSpec
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    let
      fields = renderFormSpec contractFormSpec formState
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
      { title: "Contract For Differences With Oracle"
      , description: DOOM.text "\"Party\" and \"Counterparty\" deposit 100 Ada and after 60 slots these assets are redistributed depending on the change in price of 100 Ada worth of dollars between the start and the end of the contract. If the price increases, the difference goes to \"Counterparty\"; if it decreases, the difference goes to \"Party\", up to a maximum of 100 Ada."
      , content: wrappedContentWithFooter
          formBody
          formActions
      }

