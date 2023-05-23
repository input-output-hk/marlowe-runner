module Test.Marlowe.Actus where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms(..), EventType, RiskFactors(..), Value', _fromDecimal)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (JsonDecodeError, decodeJson, jsonParser)
import Data.Array (toUnfoldable)
import Data.BigInt.Argonaut (BigInt, fromInt, fromString)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant)
import Data.Decimal (fromInt) as Decimal
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.List (List, filter)
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect.Exception (error)
import Language.Marlowe.Core.V1.Semantics (isClose, playTrace)
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId(..), Input(..), InputContent(..), Party(..), Payee(..), Payment(..), TimeInterval(..), Token(..), TransactionInput(..), TransactionOutput(..))
import Marlowe.Actus (currencyToToken, defaultRiskFactors, genContract, oracleParty)
import Marlowe.Time (unixEpoch)
import Marlowe.Utils (rewrite)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec as Spec
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "Marlowe.Actus" $ Spec.parallel do
    it "Contract generation" do
      jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/ex_pam1.json"
      json <- either (throwError <<< error) pure $ jsonParser jsonStr

      let
        (terms :: Either JsonDecodeError ContractTerms) = decodeJson json
      case terms of
        Left err -> fail (show err)
        Right contract -> do
          let
            party = Role "party"
            counterparty = Role "counterparty"
            cashFlows = genProjectedCashflows (party /\ counterparty) riskFactors contract
            marloweContract = rewrite $ genContract contract cashFlows
            token = tokenFromContractTerms contract

            payin = IDeposit party party token $ unsafePartial $ fromJust $ fromString "10000000000"
            interest = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "200000000"
            payout = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "10000000000"

            inputs = map NormalInput $ toUnfoldable
              [ payin
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , payout
              ]

            interval = TimeInterval unixEpoch unixEpoch
            output = playTrace unixEpoch marloweContract (toUnfoldable [ TransactionInput { interval, inputs } ])

          case output of
            Error err -> fail $ "PlayTrace error: " <> show err
            TransactionOutput out -> do
              shouldEqual (isClose out.txOutContract) true
              shouldEqual (totalPayments token (Party party) out.txOutPayments) (unsafePartial $ fromJust $ fromString "12000000000")
              shouldEqual (totalPayments token (Party counterparty) out.txOutPayments) (unsafePartial $ fromJust $ fromString "10000000000")

  describe "Marlowe.Actus" $ Spec.parallel do
    it "Contract generation - currency is ADA" do
      jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/ex_pam2.json"
      json <- either (throwError <<< error) pure $ jsonParser jsonStr

      let
        (terms :: Either JsonDecodeError ContractTerms) = decodeJson json
      case terms of
        Left err -> fail (show err)
        Right contract -> do
          let
            party = Role "party"
            counterparty = Role "counterparty"
            cashFlows = genProjectedCashflows (party /\ counterparty) riskFactors contract
            marloweContract = rewrite $ genContract contract cashFlows
            token = tokenFromContractTerms contract

            payin = IDeposit party party token $ unsafePartial $ fromJust $ fromString "10000000000"
            interest = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "200000000"
            payout = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "10000000000"

            inputs = map NormalInput $ toUnfoldable
              [ payin
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , payout
              ]

            interval = TimeInterval unixEpoch unixEpoch
            output = playTrace unixEpoch marloweContract (toUnfoldable [ TransactionInput { interval, inputs } ])

          case output of
            Error err -> fail $ "PlayTrace error: " <> show err
            TransactionOutput out -> do
              shouldEqual (isClose out.txOutContract) true
              shouldEqual (totalPayments token (Party party) out.txOutPayments) (unsafePartial $ fromJust $ fromString "12000000000")
              shouldEqual (totalPayments token (Party counterparty) out.txOutPayments) (unsafePartial $ fromJust $ fromString "10000000000")

    it "Contract generation - change contract role to RPL" do
      jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/ex_pam3.json"
      json <- either (throwError <<< error) pure $ jsonParser jsonStr

      let
        (terms :: Either JsonDecodeError ContractTerms) = decodeJson json
      case terms of
        Left err -> fail (show err)
        Right contract -> do
          let
            party = Role "party"
            counterparty = Role "counterparty"
            cashFlows = genProjectedCashflows (party /\ counterparty) riskFactors contract
            marloweContract = rewrite $ genContract contract cashFlows
            token = tokenFromContractTerms contract

            payin = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "10000000000"
            interest = IDeposit party party token $ unsafePartial $ fromJust $ fromString "200000000"
            payout = IDeposit party party token $ unsafePartial $ fromJust $ fromString "10000000000"

            inputs = map NormalInput $ toUnfoldable
              [ payin
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , payout
              ]

            interval = TimeInterval unixEpoch unixEpoch
            output = playTrace unixEpoch marloweContract (toUnfoldable [ TransactionInput { interval, inputs } ])

          case output of
            Error err -> fail $ "PlayTrace error: " <> show err
            TransactionOutput out -> do
              shouldEqual (isClose out.txOutContract) true
              shouldEqual (totalPayments token (Party counterparty) out.txOutPayments) (unsafePartial $ fromJust $ fromString "12000000000")
              shouldEqual (totalPayments token (Party party) out.txOutPayments) (unsafePartial $ fromJust $ fromString "10000000000")

    it "Contract generation - Oracle inputs" do
      jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/ex_pam4.json"
      json <- either (throwError <<< error) pure $ jsonParser jsonStr

      let
        (terms :: Either JsonDecodeError ContractTerms) = decodeJson json
      case terms of
        Left err -> fail (show err)
        Right contract -> do
          let
            party = Role "party"
            counterparty = Role "counterparty"
            cashFlows = genProjectedCashflows (party /\ counterparty) (defaultRiskFactors contract) contract
            marloweContract = rewrite $ genContract contract cashFlows
            token = tokenFromContractTerms contract

            exchangeRate = IChoice (ChoiceId "ADADjedTestUSD" oracleParty) (fromInt 100000000) -- 100 cents in micro currency
            payin = IDeposit party party token $ unsafePartial $ fromJust $ fromString "10000000000"
            interest = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "200000000"
            payout = IDeposit counterparty counterparty token $ unsafePartial $ fromJust $ fromString "10000000000"

            inputsPayIn = map NormalInput $ toUnfoldable
              [ exchangeRate
              , payin
              ]
            inputsInterest = map NormalInput $ toUnfoldable
              [ exchangeRate
              , interest
              ]
            inputsPayOut = map NormalInput $ toUnfoldable
              [ exchangeRate
              , payout
              ]

            intervalStart = let i = unsafePartial $ fromJust $ instant $ Milliseconds 1640995200000.0 in TimeInterval i i
            intervalEnd = let o = unsafePartial $ fromJust $ instant $ Milliseconds 1672531200000.0 in TimeInterval o o

            output = playTrace unixEpoch marloweContract
              ( toUnfoldable
                  [ TransactionInput { interval: intervalStart, inputs: inputsPayIn }
                  , TransactionInput { interval: intervalEnd, inputs: inputsInterest }
                  , TransactionInput { interval: intervalEnd, inputs: inputsPayOut }
                  ]
              )

          traceM $ show marloweContract
          case output of
            Error err -> fail $ "PlayTrace error: " <> show err
            TransactionOutput out -> do
              shouldEqual (isClose out.txOutContract) true
              shouldEqual (totalPayments token (Party party) out.txOutPayments) (unsafePartial $ fromJust $ fromString "10200000000")
              shouldEqual (totalPayments token (Party counterparty) out.txOutPayments) (unsafePartial $ fromJust $ fromString "10000000000")

tokenFromContractTerms :: ContractTerms -> Token
tokenFromContractTerms (ContractTerms { currency, settlementCurrency }) = currencyToToken $ fromMaybe "" $ settlementCurrency <|> currency

totalPayments :: Token -> Payee -> List Payment -> BigInt
totalPayments token payee = sum <<< map m <<< filter f
  where
  m (Payment _ _ t amount) | token == t = amount
  m _ = fromInt 0
  f (Payment _ pay _ _) = pay == payee

riskFactors :: EventType -> DateTime -> RiskFactors Value'
riskFactors _ _ = RiskFactors
  { o_rf_CURS: _fromDecimal $ Decimal.fromInt 1
  , o_rf_RRMO: _fromDecimal $ Decimal.fromInt 1
  , o_rf_SCMO: _fromDecimal $ Decimal.fromInt 1
  , pp_payoff: _fromDecimal $ Decimal.fromInt 0
  }
