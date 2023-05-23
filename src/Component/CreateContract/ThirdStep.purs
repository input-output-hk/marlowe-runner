module Component.CreateContract.ThirdStep where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32, bech32FromString, bech32ToString)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo)
import Component.Widgets (link)
import Contrib.Data.Foldable (foldMapFlipped)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, FormBuilder')
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Data.Array (elem)
import Data.Array as Array
import Data.BigInt.Argonaut (fromInt, toNumber, toString)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (toUpper)
import Data.Newtype (un, unwrap)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (fst, snd)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics (emptyState, evalValue)
import Language.Marlowe.Core.V1.Semantics.Types (Environment(..), Party, TimeInterval(..), Value(..))
import Language.Marlowe.Core.V1.Semantics.Types (Value(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows, defaultRiskFactors, genContract, toMarloweCashflow)
import Marlowe.Runtime.Web.Types (TxOutRef, bech32ToParty)
import Marlowe.Time (unixEpoch)
import Polyform.Batteries (rawError)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import Polyform.Validator (liftFnMMaybe) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useContext, useEffectOnce, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (WalletContext(..))

addressInput :: CardanoMultiplatformLib.Lib -> String -> String -> Maybe FieldId -> FormBuilder' Effect Bech32
addressInput cardanoMultiplatformLib label initial name = do
  let
    props =
      { initial
      , label: Just $ DOOM.text label
      , name
      , validator: requiredV' $ Validator.liftFnMMaybe (const $ pure [ "Invalid address" ]) \str -> do
          bech32FromString cardanoMultiplatformLib str
      }
  FormBuilder.textInput props

initialAddress :: String
initialAddress = "" -- "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

type FormSpec m = UseForm.Form m Unit -- JSX

type Result =
  { cashFlows :: CashFlows
  , contract :: V1.Contract
  , counterParty :: V1.Party
  , party :: V1.Party

  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  , collateralUTxOs :: Array TxOutRef
  }

type Props =
  { contractTerms :: ContractTerms
  , onSuccess :: Result -> Effect Unit
  -- , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  , connectedWallet :: WalletInfo Wallet.Api
  }

mkForm
  :: ContractTerms
  -> CardanoMultiplatformLib.Lib
  -> BootstrapForm Effect Query Result
mkForm contractTerms cardanoMultiplatformLib = FormBuilder.evalBuilder' ado
  partyAddress <- addressInput cardanoMultiplatformLib "Your address" "" $ Just (FieldId "party")
  counterPartyAddress <- addressInput cardanoMultiplatformLib "Counter-party address" initialAddress $ Just (FieldId "counter-party")
  let
    counterParty = bech32ToParty counterPartyAddress
    party = bech32ToParty partyAddress
    cashFlows =
      genProjectedCashflows
        (party /\ counterParty)
        (defaultRiskFactors contractTerms)
        contractTerms
    contract = genContract contractTerms cashFlows
  in
    { cashFlows
    , contract
    , counterParty
    , party

    -- Ugly hack
    , usedAddresses: []
    , changeAddress: partyAddress
    , collateralUTxOs: []
    }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  walletInfoCtx <- asks _.walletInfoCtx

  liftEffect $ component "CreateContract.ThridStep" \{ contractTerms, onSuccess, onDismiss, inModal } -> React.do
    possibleWalletContext <- useContext walletInfoCtx <#> map (un WalletContext <<< snd)

    let
      form = mkForm contractTerms cardanoMultiplatformLib
      onSubmit = _.result >>> case _ of
        Just ((V (Right result)) /\ _) -> do
          onSuccess $ result { usedAddresses = possibleWalletContext `foldMapFlipped` _.usedAddresses }
        _ -> do
          pure unit

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

    useEffectOnce $ do
      case possibleWalletContext, Map.lookup (FieldId "party") formState.fields of
        Just { changeAddress: Just changeAddress }, Just { onChange } -> do
          liftEffect $ onChange [ bech32ToString changeAddress ]
        _, _ -> pure unit
      pure (pure unit)

    pure $ do
      let
        fields = UseForm.renderForm form formState
        formBody =
          DOM.div { className: "form-group" } $
            fields
              <> (result >>= fst >>> un V >>> hush) `flip foldMap` \{ cashFlows } -> do
                Array.singleton
                  $ DOM.table { className: "table table-hover" }
                  $
                    [ DOM.thead {} $
                        [ DOM.tr {}
                            [ DOM.th {} [ DOOM.text "Contract Id" ]
                            , DOM.th {} [ DOOM.text "Type" ]
                            , DOM.th {} [ DOOM.text "Date" ]
                            , DOM.th {} [ DOOM.text "Amount" ]
                            , DOM.th {} [ DOOM.text "Currency" ]
                            ]
                        ]
                    , DOM.tbody {} $ Array.fromFoldable $
                        map
                          ( \cashflow ->
                              let
                                { contractId, event, paymentDay, amount, currency } = unwrap cashflow
                              in
                                [ DOM.tr {}
                                    [ DOM.td {} [ DOOM.text contractId ]
                                    , DOM.td {} [ DOOM.text $ show event ]
                                    , DOM.td {} [ DOOM.text <$> hush (formatDateTime "YYYY-DD-MM HH:mm:ss:SSS" paymentDay) ]
                                    , DOM.td {}
                                        [ DOOM.text $ do
                                            -- empty state and environment at contract creation
                                            let
                                              environment = Environment { timeInterval: TimeInterval unixEpoch unixEpoch }
                                              state = emptyState
                                            show <<< (_ / 1000000.0) <<< toNumber $ evalValue environment state amount
                                        ]
                                    , DOM.td {} [ DOOM.text $ if elem (toUpper currency) [ "", "ADA" ] then "₳" else currency ]
                                    ]
                                ]
                          )
                          (map toMarloweCashflow cashFlows)
                    -- =======
                    --                   , DOM.tbody {} $ Array.fromFoldable $ cashFlows <#> \cashFlow -> do
                    --                       let
                    --                         cf = unwrap $ toMarloweCashflow cashFlow
                    --                       DOM.tr {}
                    --                         [ DOM.td {} [ DOOM.text cf.contractId ]
                    --                         , DOM.td {} [ DOOM.text $ show cf.event ]
                    --                         , DOM.td {} [ DOOM.text <$> hush (formatDateTime "YYYY-DD-MM HH:mm:ss:SSS" cf.paymentDay) ]
                    --                         , DOM.td {}
                    --                             [ DOOM.text $ fromMaybe "" $
                    --                                 if elem cf.currency currenciesWith6Decimals then show <$> (((_ / 1000000.0) <<< toNumber) <$> evalVal cf.amount)
                    --                                 else toString <$> (evalVal $ DivValue cf.amount (Constant $ fromInt 1000000))
                    --                             ]
                    --                         , DOM.td {} [ DOOM.text $ if cf.currency == "" then "₳" else cf.currency ]
                    --                         ]
                    -- >>>>>>> e42a028 (Ongoing work on the contract wizzard):src/Component/CreateContract/ThirdStep.purs
                    ]
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
                    Just ((V (Right _)) /\ _) -> false
                    _ -> true
                { className: "btn btn-primary"
                , onClick: onSubmit'
                , disabled
                }
              [ R.text "Submit" ]
          ]

      if inModal then modal
        { title: R.text "Add contract | Step 3 of 4"
        , onDismiss
        , body: formBody
        , footer: formActions
        , size: Modal.Large
        }
      else
        formBody

