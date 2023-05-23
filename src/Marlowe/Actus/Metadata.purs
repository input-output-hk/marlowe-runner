module Marlowe.Actus.Metadata where

import Prelude

import Actus.Domain.ContractTerms (ContractTerms(..), decodeCycle, decodeDecimal, encodeCycle, encodeDecimal)
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, encodeJson, jsonEmptyObject, (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Bifunctor (rmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, toDateTime)
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Language.Marlowe.Core.V1.Semantics.Types (Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types as Runtime
import Marlowe.Time (instantFromJson, instantToJson)

actusMetadataKey :: Int
actusMetadataKey = 124 -- FIXME: define a proper key

newtype Metadata = Metadata
  { contractTerms :: ContractTerms
  , party :: Party -- TODO: really need in metadata?
  , counterParty :: Party -- TODO: really need in metadata?
  }

-- Because there is a limit for a single bytestring length
-- on the blockchain, we need to split the metadata into
-- two parts so they fit ;-)
newtype PartyParts = PartyParts Party

instance encodeJson :: EncodeJson PartyParts where
  encodeJson (PartyParts (V1.Role role)) = encodeJson { role }
  encodeJson (PartyParts (V1.Address address)) = do
    let
      { before, after } = String.splitAt 64 address
    encodeJson { address: { before, after } }

instance decodeJson :: DecodeJson PartyParts where
  decodeJson json = do
    obj <- decodeJson json
    let
      decodeRole = do
        role <- obj .: "role"
        pure $ PartyParts $ V1.Role role

      decodeAddress = do
        address <- obj .: "address"
        before <- address .: "before"
        after <- address .: "after"
        pure $ PartyParts $ V1.Address $ before <> after
    decodeRole <|> decodeAddress

derive instance Eq Metadata
derive instance Generic Metadata _
derive instance Newtype Metadata _

encodeDateTime :: DateTime -> Json
encodeDateTime = instantToJson <<< fromDateTime

decodeDateTime :: Json -> Either JsonDecodeError DateTime
decodeDateTime json = rmap toDateTime (instantFromJson json)

encodeMetadataObject :: Metadata -> Object Json
encodeMetadataObject metadata = do
  let
    json = encodeJson metadata
  case decodeJson json of
    Left _ -> Object.empty
    Right obj -> obj

-- Using acronyms from `actus-dictionary-terms.json` for encoding/decoding
-- https://github.com/actusfrf/actus-dictionary
instance EncodeJson Metadata where
  encodeJson (Metadata { contractTerms: ContractTerms ct, party, counterParty }) =
    "cid" := ct.contractId
      ~> "ct" := ct.contractType
      ~> "cntrl" := ct.contractRole
      ~> "curs" :=? ct.settlementCurrency
      ~>? "ied" :=? (encodeDateTime <$> ct.initialExchangeDate)
      ~>? "dcc" :=? ct.dayCountConvention
      ~>? "cal" :=? ct.scheduleConfig.calendar
      ~>? "emoc" :=? ct.scheduleConfig.endOfMonthConvention
      ~>? "bdc" :=? ct.scheduleConfig.businessDayConvention
      ~>? "sd" := (encodeDateTime ct.statusDate)
      ~> "moc" :=? ct.marketObjectCode
      ~>? "prf" :=? ct.contractPerformance
      ~>? "cetc" :=? ct.creditEventTypeCovered
      ~>? "cecv" :=? (encodeDecimal <$> ct.coverageOfCreditEnhancement)
      ~>? "cege" :=? ct.guaranteedExposure
      ~>? "fecl" :=? (encodeCycle <$> ct.cycleOfFee)
      ~>? "feanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfFee)
      ~>? "feac" :=? (encodeDecimal <$> ct.feeAccrued)
      ~>? "feb" :=? ct.feeBasis
      ~>? "fer" :=? (encodeDecimal <$> ct.feeRate)
      ~>? "ipanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfInterestPayment)
      ~>? "ipcl" :=? (encodeCycle <$> ct.cycleOfInterestPayment)
      ~>? "ipac" :=? (encodeDecimal <$> ct.accruedInterest)
      ~>? "ipced" :=? (encodeDateTime <$> ct.capitalizationEndDate)
      ~>? "ipcbanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfInterestCalculationBase)
      ~>? "ipcbcl" :=? (encodeCycle <$> ct.cycleOfInterestCalculationBase)
      ~>? "ipcb" :=? ct.interestCalculationBase
      ~>? "ipcba" :=? (encodeDecimal <$> ct.interestCalculationBaseAmount)
      ~>? "ipnr" :=? (encodeDecimal <$> ct.nominalInterestRate)
      ~>? "ipnr2" :=? (encodeDecimal <$> ct.nominalInterestRate2)
      ~>? "scip" :=? (encodeDecimal <$> ct.interestScalingMultiplier)
      ~>? "md" :=? (encodeDateTime <$> ct.maturityDate)
      ~>? "ad" :=? (encodeDateTime <$> ct.amortizationDate)
      ~>? "xd" :=? (encodeDateTime <$> ct.exerciseDate)
      ~>? "nt" :=? (encodeDecimal <$> ct.notionalPrincipal)
      ~>? "pdied" :=? (encodeDecimal <$> ct.premiumDiscountAtIED)
      ~>? "pranx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfPrincipalRedemption)
      ~>? "prcl" :=? (encodeCycle <$> ct.cycleOfPrincipalRedemption)
      ~>? "prnxt" :=? (encodeDecimal <$> ct.nextPrincipalRedemptionPayment)
      ~>? "prd" :=? (encodeDateTime <$> ct.purchaseDate)
      ~>? "pprd" :=? (encodeDecimal <$> ct.priceAtPurchaseDate)
      ~>? "td" :=? (encodeDateTime <$> ct.terminationDate)
      ~>? "ptd" :=? (encodeDecimal <$> ct.priceAtTerminationDate)
      ~>? "qt" :=? (encodeDecimal <$> ct.quantity)
      ~>? "cur" :=? ct.currency
      ~>? "cur2" :=? ct.currency2
      ~>? "scsd" :=? (encodeDecimal <$> ct.scalingIndexAtStatusDate)
      ~>? "scanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfScalingIndex)
      ~>? "sccl" :=? (encodeCycle <$> ct.cycleOfScalingIndex)
      ~>? "scef" :=? ct.scalingEffect
      ~>? "sccdd" :=? (encodeDecimal <$> ct.scalingIndexAtContractDealDate)
      ~>? "scmo" :=? ct.marketObjectCodeOfScalingIndex
      ~>? "scnt" :=? (encodeDecimal <$> ct.notionalScalingMultiplier)
      ~>? "opcl" :=? (encodeCycle <$> ct.cycleOfOptionality)
      ~>? "opanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfOptionality)
      ~>? "optp" :=? ct.optionType
      ~>? "ops1" :=? (encodeDecimal <$> ct.optionStrike1)
      ~>? "opxt" :=? ct.optionExerciseType
      ~>? "stp" :=? (encodeCycle <$> ct.settlementPeriod)
      ~>? "ds" :=? ct.deliverySettlement
      ~>? "xa" :=? (encodeDecimal <$> ct.exerciseAmount)
      ~>? "pfut" :=? (encodeDecimal <$> ct.futuresPrice)
      ~>? "pyrt" :=? (encodeDecimal <$> ct.penaltyRate)
      ~>? "pytp" :=? ct.penaltyType
      ~>? "ppef" :=? ct.prepaymentEffect
      ~>? "rrcl" :=? (encodeCycle <$> ct.cycleOfRateReset)
      ~>? "rranx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfRateReset)
      ~>? "rrnxt" :=? (encodeDecimal <$> ct.nextResetRate)
      ~>? "rrsp" :=? (encodeDecimal <$> ct.rateSpread)
      ~>? "rrmlt" :=? (encodeDecimal <$> ct.rateMultiplier)
      ~>? "rrpf" :=? (encodeDecimal <$> ct.periodFloor)
      ~>? "rrpc" :=? (encodeDecimal <$> ct.periodCap)
      ~>? "rrlc" :=? (encodeDecimal <$> ct.lifeCap)
      ~>? "rrlf" :=? (encodeDecimal <$> ct.lifeFloor)
      ~>? "rrmo" :=? ct.marketObjectCodeOfRateReset
      ~>? "dvcl" :=? (encodeCycle <$> ct.cycleOfDividendPayment)
      ~>? "dvanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfDividendPayment)
      ~>? "dvnxt" :=? (encodeDecimal <$> ct.nextDividendPaymentAmount)
      ~>? "pa1" := PartyParts party
      ~> "cp1" := PartyParts counterParty
      ~> jsonEmptyObject

instance DecodeJson Metadata where
  decodeJson json = Metadata <$> do
    obj <- decodeJson json
    contractId <- obj .: "cid"
    contractType <- obj .: "ct"
    contractRole <- obj .: "cntrl"
    settlementCurrency <- obj .:? "curs" :: Either JsonDecodeError (Maybe String)
    initialExchangeDate <- Decoders.getFieldOptional' decodeDateTime obj "ied"
    dayCountConvention <- obj .:? "dcc"
    calendar <- obj .:? "cal"
    endOfMonthConvention <- obj .:? "emoc"
    businessDayConvention <- obj .:? "bdc"
    statusDate <- obj .: "sd" >>= decodeDateTime
    marketObjectCode <- obj .:? "moc"
    contractPerformance <- obj .:? "prf"
    creditEventTypeCovered <- obj .:? "cetc"
    coverageOfCreditEnhancement <- Decoders.getFieldOptional' decodeDecimal obj "cecv"
    guaranteedExposure <- obj .:? "cege"
    cycleOfFee <- rmap (_ >>= decodeCycle) (obj .:? "fecl")
    cycleAnchorDateOfFee <- Decoders.getFieldOptional' decodeDateTime obj "feanx"
    feeAccrued <- Decoders.getFieldOptional' decodeDecimal obj "feac"
    feeBasis <- obj .:? "feb"
    feeRate <- Decoders.getFieldOptional' decodeDecimal obj "fer"
    cycleAnchorDateOfInterestPayment <- Decoders.getFieldOptional' decodeDateTime obj "ipanx"
    cycleOfInterestPayment <- rmap (_ >>= decodeCycle) (obj .:? "ipcl")
    accruedInterest <- Decoders.getFieldOptional' decodeDecimal obj "ipac"
    capitalizationEndDate <- Decoders.getFieldOptional' decodeDateTime obj "ipced"
    cycleAnchorDateOfInterestCalculationBase <- Decoders.getFieldOptional' decodeDateTime obj "ipcbanx"
    cycleOfInterestCalculationBase <- rmap (_ >>= decodeCycle) (obj .:? "ipcbcl")
    interestCalculationBase <- obj .:? "ipcb"
    interestCalculationBaseAmount <- Decoders.getFieldOptional' decodeDecimal obj "ipcba"
    nominalInterestRate <- Decoders.getFieldOptional' decodeDecimal obj "ipnr"
    nominalInterestRate2 <- Decoders.getFieldOptional' decodeDecimal obj "ipnr2"
    interestScalingMultiplier <- Decoders.getFieldOptional' decodeDecimal obj "scip"
    maturityDate <- Decoders.getFieldOptional' decodeDateTime obj "md"
    amortizationDate <- Decoders.getFieldOptional' decodeDateTime obj "ad"
    exerciseDate <- Decoders.getFieldOptional' decodeDateTime obj "xd"
    notionalPrincipal <- Decoders.getFieldOptional' decodeDecimal obj "nt"
    premiumDiscountAtIED <- Decoders.getFieldOptional' decodeDecimal obj "pdied"
    cycleAnchorDateOfPrincipalRedemption <- Decoders.getFieldOptional' decodeDateTime obj "pranx"
    cycleOfPrincipalRedemption <- rmap (_ >>= decodeCycle) (obj .:? "prcl")
    nextPrincipalRedemptionPayment <- Decoders.getFieldOptional' decodeDecimal obj "prnxt"
    purchaseDate <- Decoders.getFieldOptional' decodeDateTime obj "prd"
    priceAtPurchaseDate <- Decoders.getFieldOptional' decodeDecimal obj "pprd"
    terminationDate <- Decoders.getFieldOptional' decodeDateTime obj "td"
    priceAtTerminationDate <- Decoders.getFieldOptional' decodeDecimal obj "ptd"
    quantity <- Decoders.getFieldOptional' decodeDecimal obj "qt"
    currency <- obj .:? "cur"
    currency2 <- obj .:? "cur2"
    scalingIndexAtStatusDate <- Decoders.getFieldOptional' decodeDecimal obj "scsd"
    cycleAnchorDateOfScalingIndex <- Decoders.getFieldOptional' decodeDateTime obj "scanx"
    cycleOfScalingIndex <- rmap (_ >>= decodeCycle) (obj .:? "sccl")
    scalingEffect <- obj .:? "scef"
    scalingIndexAtContractDealDate <- Decoders.getFieldOptional' decodeDecimal obj "sccdd"
    marketObjectCodeOfScalingIndex <- obj .:? "scmo"
    notionalScalingMultiplier <- Decoders.getFieldOptional' decodeDecimal obj "scnt"
    cycleOfOptionality <- rmap (_ >>= decodeCycle) (obj .:? "opcl")
    cycleAnchorDateOfOptionality <- Decoders.getFieldOptional' decodeDateTime obj "opanx"
    optionType <- obj .:? "optp"
    optionStrike1 <- Decoders.getFieldOptional' decodeDecimal obj "ops1"
    optionExerciseType <- obj .:? "opxt"
    settlementPeriod <- rmap (_ >>= decodeCycle) (obj .:? "stp")
    deliverySettlement <- obj .:? "ds"
    exerciseAmount <- Decoders.getFieldOptional' decodeDecimal obj "xa"
    futuresPrice <- Decoders.getFieldOptional' decodeDecimal obj "pfut"
    penaltyRate <- Decoders.getFieldOptional' decodeDecimal obj "pyrt"
    penaltyType <- obj .:? "pytp"
    prepaymentEffect <- obj .:? "ppef"
    cycleOfRateReset <- rmap (_ >>= decodeCycle) (obj .:? "rrcl")
    cycleAnchorDateOfRateReset <- Decoders.getFieldOptional' decodeDateTime obj "rranx"
    nextResetRate <- Decoders.getFieldOptional' decodeDecimal obj "rrnxt"
    rateSpread <- Decoders.getFieldOptional' decodeDecimal obj "rrsp"
    rateMultiplier <- Decoders.getFieldOptional' decodeDecimal obj "rrmlt"
    periodFloor <- Decoders.getFieldOptional' decodeDecimal obj "rrpf"
    periodCap <- Decoders.getFieldOptional' decodeDecimal obj "rrpc"
    lifeCap <- Decoders.getFieldOptional' decodeDecimal obj "rrlc"
    lifeFloor <- Decoders.getFieldOptional' decodeDecimal obj "rrlf"
    marketObjectCodeOfRateReset <- obj .:? "rrmo"
    cycleOfDividendPayment <- rmap (_ >>= decodeCycle) (obj .:? "dvcl")
    cycleAnchorDateOfDividendPayment <- Decoders.getFieldOptional' decodeDateTime obj "dvanx"
    nextDividendPaymentAmount <- Decoders.getFieldOptional' decodeDecimal obj "dvnxt"
    PartyParts party <- obj .: "pa1"
    PartyParts counterParty <- obj .: "cp1"
    pure $
      { contractTerms: ContractTerms
          { contractId
          , contractType
          , contractRole
          , settlementCurrency
          , initialExchangeDate
          , dayCountConvention
          , scheduleConfig: { calendar, endOfMonthConvention, businessDayConvention }
          , statusDate
          , marketObjectCode
          , contractPerformance
          , creditEventTypeCovered
          , coverageOfCreditEnhancement
          , guaranteedExposure
          , cycleOfFee
          , cycleAnchorDateOfFee
          , feeAccrued
          , feeBasis
          , feeRate
          , cycleAnchorDateOfInterestPayment
          , cycleOfInterestPayment
          , accruedInterest
          , capitalizationEndDate
          , cycleAnchorDateOfInterestCalculationBase
          , cycleOfInterestCalculationBase
          , interestCalculationBase
          , interestCalculationBaseAmount
          , nominalInterestRate
          , nominalInterestRate2
          , interestScalingMultiplier
          , maturityDate
          , amortizationDate
          , exerciseDate
          , notionalPrincipal
          , premiumDiscountAtIED
          , cycleAnchorDateOfPrincipalRedemption
          , cycleOfPrincipalRedemption
          , nextPrincipalRedemptionPayment
          , purchaseDate
          , priceAtPurchaseDate
          , terminationDate
          , priceAtTerminationDate
          , quantity
          , currency
          , currency2
          , scalingIndexAtStatusDate
          , cycleAnchorDateOfScalingIndex
          , cycleOfScalingIndex
          , scalingEffect
          , scalingIndexAtContractDealDate
          , marketObjectCodeOfScalingIndex
          , notionalScalingMultiplier
          , cycleOfOptionality
          , cycleAnchorDateOfOptionality
          , optionType
          , optionStrike1
          , optionExerciseType
          , settlementPeriod
          , deliverySettlement
          , exerciseAmount
          , futuresPrice
          , penaltyRate
          , penaltyType
          , prepaymentEffect
          , cycleOfRateReset
          , cycleAnchorDateOfRateReset
          , nextResetRate
          , rateSpread
          , rateMultiplier
          , periodFloor
          , periodCap
          , lifeCap
          , lifeFloor
          , marketObjectCodeOfRateReset
          , cycleOfDividendPayment
          , cycleAnchorDateOfDividendPayment
          , nextDividendPaymentAmount
          }
      , party
      , counterParty
      }

decodeMetadata :: Runtime.Metadata -> Maybe Metadata
decodeMetadata (Runtime.Metadata md) = Map.lookup actusMetadataKey md >>= decodeJson >>> hush

-- | Grab metadata from `Runtime.Contract` or from `Runtime.ContractHeader`.
fromRuntimeResource :: forall resource r. Newtype resource { metadata :: Runtime.Metadata | r } => resource -> Maybe Metadata
fromRuntimeResource resource = do
  let
    { metadata } = unwrap resource
  decodeMetadata metadata

