-- | Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Marlowe.Actus
  ( CashFlows
  , RiskFactorsMarlowe
  , genContract
  , genContract'
  , defaultRiskFactors
  , toMarloweCashflow
  , toMarloweValue
  , currencyToToken
  , oracleParty
  , module Exports
  ) where

import Prelude

import Actus.Domain (CashFlow(..), ContractState, ContractTerms(..), Observation'(..), RiskFactors(..), Value'(..))
import Actus.Domain.BusinessEvents (EventType(..))
import Actus.Domain.ContractTerms (CR(..))
import Data.BigInt.Argonaut (fromInt, fromString)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime, unInstant)
import Data.Foldable (foldl)
import Data.Int (round)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Seconds(..), convertDuration)
import Language.Marlowe.Core.V1.Semantics.Types (Action(..), Bound(..), Case(..), ChoiceId(..), Contract(..), Observation(..), Party(..), Payee(..), Token(..), Value(..), ValueId(..))
import Marlowe.Actus.Metadata (Metadata(..)) as Exports
import Marlowe.Utils (rewrite)

type CashFlows = List (CashFlow Value' Party)
type ContractStateMarlowe = ContractState Value'
type RiskFactorsMarlowe = RiskFactors Value'

toMarloweValue :: Value' -> Value
toMarloweValue (Constant' n) = Constant n
toMarloweValue (NegValue' n) = NegValue $ toMarloweValue n
toMarloweValue (AddValue' a b) = AddValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (SubValue' a b) = SubValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (MulValue' a b) = MulValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (DivValue' a b) = DivValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (Cond' o a b) = Cond (toMarloweObservation o) (toMarloweValue a) (toMarloweValue b)
toMarloweValue (UseValue' i) = UseValue i

toMarloweObservation :: Observation' -> Observation
toMarloweObservation (AndObs' a b) = AndObs (toMarloweObservation a) (toMarloweObservation b)
toMarloweObservation (OrObs' a b) = OrObs (toMarloweObservation a) (toMarloweObservation b)
toMarloweObservation (NotObs' a) = NotObs $ toMarloweObservation a
toMarloweObservation (ValueGE' a b) = ValueGE (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueGT' a b) = ValueGT (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueLT' a b) = ValueLT (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueLE' a b) = ValueLE (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueEQ' a b) = ValueEQ (toMarloweValue a) (toMarloweValue b)
toMarloweObservation TrueObs' = TrueObs
toMarloweObservation FalseObs' = FalseObs

toMarloweCashflow :: CashFlow Value' Party -> CashFlow Value Party
toMarloweCashflow
  ( CashFlow
      { contractId
      , party
      , counterparty
      , paymentDay
      , calculationDay
      , event
      , contractRole
      , amount
      , notional
      , currency
      }
  ) = CashFlow
  { contractId
  , party
  , counterparty
  , paymentDay
  , calculationDay
  , event
  , contractRole
  , amount: toMarloweValue amount
  , notional: toMarloweValue notional
  , currency
  }

-- | 'genContract' generates contract terms from projected cash-flows
genContract
  :: ContractTerms
  -- | List of projected cash-flows
  -> CashFlows
  ->
  -- | Marlowe contract
  Contract
genContract contractTerms cashFlows = genContract' contractTerms $ reverse (map toMarloweCashflow cashFlows)

-- | 'genContract' generates contract terms from projected cash-flows
genContract'
  :: ContractTerms
  -- | List of projected cash-flows
  -> List (CashFlow Value Party)
  ->
  -- | Marlowe contract
  Contract
genContract' contractTerms cashFlows = rewrite $ foldl (generator contractTerms) Close cashFlows
  where
  generator :: ContractTerms -> Contract -> CashFlow Value Party -> Contract
  generator
    ( ContractTerms
        { currency: Just currency'
        , settlementCurrency: Just settlementCurrency'
        }
    )
    continuation
    cashFlow@
      ( CashFlow
          { calculationDay
          , paymentDay
          }
      )
    | hasRiskFactor cashFlow =
        let
          label = currency' <> settlementCurrency'
          choiceId = ChoiceId label oracleParty
        in
          When [] (fromDateTime calculationDay) $
            When
              [ Case
                  (Choice choiceId [ Bound (fromInt 0) (fromMaybe (fromInt 1000000000) $ fromString "1000000000000000000") ]) $
                  Let (ValueId $ label <> dateTimeToTimestamp calculationDay) (ChoiceValue (ChoiceId label oracleParty)) (stub continuation cashFlow)
              ]
              (fromDateTime paymentDay) -- precondition: calculationDay < paymentDay
              Close
  generator _ continuation cashFlow = stub continuation cashFlow

  stub continuation (CashFlow { party, counterparty, currency, amount, paymentDay, contractRole, event }) =
    let
      negAmount = NegValue amount
      token = currencyToToken currency
      timeout = fromDateTime paymentDay
    in
      case event, contractRole of
        IED, CR_RPA -> invoice party counterparty token negAmount timeout continuation
        IED, CR_RPL -> invoice counterparty party token amount timeout continuation
        IP, CR_RPA -> invoice counterparty party token amount timeout continuation
        IP, CR_RPL -> invoice party counterparty token negAmount timeout continuation
        MD, CR_RPA -> invoice counterparty party token amount timeout continuation
        MD, CR_RPL -> invoice party counterparty token negAmount timeout continuation
        _, _ -> rewrite $ If ((Constant $ fromInt 0) `ValueLT` amount)
          (invoice counterparty party token amount timeout continuation)
          ( If (amount `ValueLT` (Constant $ fromInt 0))
              (invoice party counterparty token negAmount timeout continuation)
              continuation
          )

  invoice :: Party -> Party -> Token -> Value -> Instant -> Contract -> Contract
  invoice a b token amount timeout continue =
    -- TODO: Use MerkleizedCase instead
    When [ Case (Deposit a a token amount) (Pay a (Party b) token amount continue) ] timeout Close

currencyToToken :: String -> Token
currencyToToken "DjedTestUSD" = djed
currencyToToken "USD" = djed
currencyToToken "ADA" = ada
currencyToToken i = Token "" i

ada :: Token
ada = Token "" ""

djed :: Token
djed = Token "9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe" "Djed_testMicroUSD"

hasRiskFactor :: CashFlow Value Party -> Boolean
hasRiskFactor (CashFlow { amount }) = hasRiskFactor' amount
  where
  hasRiskFactor' :: Value -> Boolean
  hasRiskFactor' (ChoiceValue _) = true
  hasRiskFactor' (Constant _) = false
  hasRiskFactor' (AvailableMoney _ _) = false
  hasRiskFactor' (UseValue _) = true
  hasRiskFactor' (AddValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (SubValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (MulValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (DivValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (NegValue a) = hasRiskFactor' a
  hasRiskFactor' TimeIntervalStart = false
  hasRiskFactor' TimeIntervalEnd = false
  hasRiskFactor' (Cond _ a b) = hasRiskFactor' a || hasRiskFactor' b

oracleParty :: Party
oracleParty = Address "addr_test1qp5e9feu4hkp4qwvgqasq02na05z3eg33zzjquf2d86e6qzznwng4gtlladnxm7d486psa003jy6dv230t82rvv3pflq62lz84" -- Oracle preprod

dateTimeToTimestamp :: DateTime -> String
dateTimeToTimestamp dateTime =
  let
    instant = fromDateTime dateTime
    millis = unInstant instant
    (Seconds seconds) = convertDuration millis
  in
    show $ round seconds

defaultRiskFactors :: ContractTerms -> EventType -> DateTime -> RiskFactorsMarlowe
defaultRiskFactors contractTerms _ eventTime =
  RiskFactors
    { o_rf_CURS: case contractTerms of
        (ContractTerms { currency: Just currency', settlementCurrency: Just settlementCurrency' }) | currency' /= settlementCurrency' ->
          DivValue'
            (UseValue' $ ValueId (currency' <> settlementCurrency' <> dateTimeToTimestamp eventTime)) -- value in micro cents
            (Constant' $ fromInt 100) -- therefore we divide by cents
        _ -> one
    , o_rf_RRMO: UseValue' (ValueId "rrmo") -- TODO: add to oracle
    , o_rf_SCMO: UseValue' (ValueId "scmo") -- TODO: add to oracle
    , pp_payoff: UseValue' (ValueId "pp") -- TODO: add to oracle
    }
