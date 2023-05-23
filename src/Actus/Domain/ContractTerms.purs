module Actus.Domain.ContractTerms where

import Prelude

import Contrib.Data.Argonaut (decodeJsonEnumWith, decodeFromString, encodeJsonEnumWith)
import Contrib.Data.Argonaut.Decode.Record.Field (askField, askObject, execRecordBuilderM, insertProp, liftEither, (:=), (:=!), (:=?), (:=?!))
import Contrib.Data.String (decodeEnumWith, tryStripPrefix)
import Control.Alt ((<|>))
import Control.Monad.Indexed.Qualified as Ix
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, fromObject, fromString)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array.NonEmpty as NA
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.DateTime (DateTime)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either, note)
import Data.Enum (class BoundedEnum, class Enum, upFromIncluding)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String (joinWith) as String
import Data.String.Regex (match)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (trimStart) as String
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Record as Record
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

-- |ContractType
data CT
  = PAM -- ^ Principal at maturity
  | LAM -- ^ Linear amortizer
  | NAM -- ^ Negative amortizer
  | ANN -- ^ Annuity

derive instance Generic CT _
derive instance Eq CT
derive instance Ord CT

instance Show CT where
  show = genericShow

instance Enum CT where
  succ = genericSucc
  pred = genericPred

instance Bounded CT where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum CT where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson CT where
  encodeJson = encodeJsonEnumWith identity

instance DecodeJson CT where
  decodeJson = decodeJsonEnumWith identity

-- |ContractRole
data CR
  = CR_RPA -- ^ Real position asset
  | CR_RPL -- ^ Real position liability
  | CR_CLO -- ^ Role of a collateral
  | CR_CNO -- ^ Role of a close-out-netting
  | CR_COL -- ^ Role of an underlying to a collateral
  | CR_LG -- ^ Long position
  | CR_ST -- ^ Short position
  | CR_BUY -- ^ Protection buyer
  | CR_SEL -- ^ Protection seller
  | CR_RFL -- ^ Receive first leg
  | CR_PFL -- ^ Pay first leg
  | CR_RF -- ^ Receive fix leg
  | CR_PF -- ^ Pay fix leg

derive instance Generic CR _
derive instance Eq CR
derive instance Ord CR

instance Show CR where
  show = genericShow

instance Enum CR where
  succ = genericSucc
  pred = genericPred

instance Bounded CR where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum CR where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson CR where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "CR_")

instance DecodeJson CR where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "CR_")

-- |DayCountConvention
data DCC
  = DCC_A_AISDA -- ^ Actual/Actual ISDA
  | DCC_A_360 -- ^ Actual/360
  | DCC_A_365 -- ^ Actual/365
  | DCC_E30_360ISDA -- ^ 30E/360 ISDA
  | DCC_E30_360 -- ^ 30E/360
  | DCC_B_252 -- ^ Business / 252

derive instance Generic DCC _
derive instance Eq DCC
derive instance Ord DCC

instance Enum DCC where
  succ = genericSucc
  pred = genericPred

instance Bounded DCC where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum DCC where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance Show DCC where
  show DCC_A_AISDA = "AA"
  show DCC_A_360 = "A360"
  show DCC_A_365 = "A365"
  show DCC_E30_360ISDA = "30E360ISDA"
  show DCC_E30_360 = "30E360"
  show DCC_B_252 = "B252"

instance EncodeJson DCC where
  encodeJson DCC_A_AISDA = fromString "AA"
  encodeJson DCC_A_360 = fromString "A360"
  encodeJson DCC_A_365 = fromString "A365"
  encodeJson DCC_E30_360ISDA = fromString "30E360ISDA"
  encodeJson DCC_E30_360 = fromString "30E360"
  encodeJson DCC_B_252 = fromString "B252"

instance DecodeJson DCC where
  decodeJson = decodeFromString decode
    where
    decode "AA" = pure DCC_A_AISDA
    decode "A360" = pure DCC_A_360
    decode "A365" = pure DCC_A_365
    decode "30E360ISDA" = pure DCC_E30_360ISDA
    decode "30E360" = pure DCC_E30_360
    decode "B252" = pure DCC_B_252
    decode _ = Nothing

-- -- |EndOfMonthConvention
data EOMC
  = EOMC_EOM -- ^ End of month
  | EOMC_SD -- ^ Same day

derive instance Generic EOMC _
derive instance Eq EOMC
derive instance Ord EOMC
instance Show EOMC where
  show = genericShow

instance Enum EOMC where
  succ = genericSucc
  pred = genericPred

instance Bounded EOMC where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum EOMC where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson EOMC where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "EOMC_")

instance DecodeJson EOMC where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "EOMC_")

-- -- |BusinessDayConvention
data BDC
  = BDC_NULL -- ^ No shift
  | BDC_SCF -- ^ Shift/calculate following
  | BDC_SCMF -- ^ Shift/calculate modified following
  | BDC_CSF -- ^ Calculate/shift following
  | BDC_CSMF -- ^ Calculate/shift modified following
  | BDC_SCP -- ^ Shift/calculate preceding
  | BDC_SCMP -- ^ Shift/calculate modified preceding
  | BDC_CSP -- ^ Calculate/shift preceding
  | BDC_CSMP -- ^ Calculate/shift modified preceding

derive instance Generic BDC _
derive instance Eq BDC
derive instance Ord BDC

instance Show BDC where
  show = genericShow

instance Enum BDC where
  succ = genericSucc
  pred = genericPred

instance Bounded BDC where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum BDC where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson BDC where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "BDC_")

instance DecodeJson BDC where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "BDC_")

data Calendar
  = CLDR_NC -- ^ No calendar
  | CLDR_MF -- ^ Monday to Friday

derive instance Generic Calendar _
derive instance Eq Calendar
derive instance Ord Calendar

instance Show Calendar where
  show = genericShow

instance Enum Calendar where
  succ = genericSucc
  pred = genericPred

instance Bounded Calendar where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Calendar where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson Calendar where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "CLDR_")

instance DecodeJson Calendar where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "CLDR_")

type ScheduleConfig =
  { calendar :: Maybe Calendar
  , endOfMonthConvention :: Maybe EOMC
  , businessDayConvention :: Maybe BDC
  }

-- |ContractPerformance
data PRF
  = PRF_PF -- ^ Performant
  | PRF_DL -- ^ Delayed
  | PRF_DQ -- ^ Delinquent
  | PRF_DF -- ^ Default

derive instance Generic PRF _
derive instance Eq PRF
derive instance Ord PRF

instance Show PRF where
  show = genericShow

instance Enum PRF where
  succ = genericSucc
  pred = genericPred

instance Bounded PRF where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum PRF where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson PRF where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "PRF_")

instance DecodeJson PRF where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "PRF_")

-- |CreditEventTypeCovered
data CETC
  = CETC_DL -- ^ Delayed
  | CETC_DQ -- ^ Delinquent
  | CETC_DF -- ^ Default

derive instance Generic CETC _
derive instance Eq CETC
derive instance Ord CETC

instance Show CETC where
  show = genericShow

instance Enum CETC where
  succ = genericSucc
  pred = genericPred

instance Bounded CETC where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum CETC where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson CETC where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "CETC_")

instance DecodeJson CETC where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "CETC_")

-- |GuaranteedExposure
data CEGE
  = CEGE_NO -- ^ Nominal value
  | CEGE_NI -- ^ Nominal value plus interest

derive instance Generic CEGE _
derive instance Eq CEGE
derive instance Ord CEGE

instance Show CEGE where
  show = genericShow

instance Enum CEGE where
  succ = genericSucc
  pred = genericPred

instance Bounded CEGE where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum CEGE where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson CEGE where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "CEGE_")

instance DecodeJson CEGE where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "CEGE_")

-- |FeeBasis
data FEB
  = FEB_A -- ^ Absolute value
  | FEB_N -- ^ Notional of underlying

derive instance Generic FEB _
derive instance Eq FEB
derive instance Ord FEB

instance Show FEB where
  show = genericShow

instance Enum FEB where
  succ = genericSucc
  pred = genericPred

instance Bounded FEB where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum FEB where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson FEB where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "FEB_")

instance DecodeJson FEB where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "FEB_")

-- |InterestCalculationBase
data IPCB
  = IPCB_NT -- ^ Calculation base always equals to NT
  | IPCB_NTIED -- ^ Notional remains constant amount as per IED
  | IPCB_NTL -- ^ Calculation base is notional base laged

derive instance Generic IPCB _
derive instance Eq IPCB
derive instance Ord IPCB

instance Show IPCB where
  show = genericShow

instance Enum IPCB where
  succ = genericSucc
  pred = genericPred

instance Bounded IPCB where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum IPCB where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson IPCB where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "IPCB_")

instance DecodeJson IPCB where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "IPCB_")

-- |ScalingEffect
data SCEF
  = SE_OOO -- ^ No scaling
  | SE_IOO -- ^ Only interest payments scaled
  | SE_ONO -- ^ Only nominal payments scaled
  | SE_OOM -- ^ Only maximum deferred amount scaled
  | SE_INO -- ^ Interest and nominal payments scaled
  | SE_ONM -- ^ Nominal and maximum deferred amount scaled
  | SE_IOM -- ^ Interest and maximum deferred amount scaled
  | SE_INM -- ^ Interest, nominal and maximum deferred amount scaled

derive instance Generic SCEF _
derive instance Eq SCEF
derive instance Ord SCEF

instance Show SCEF where
  show = tryStripPrefix (Pattern "SE_") <<< genericShow

instance Enum SCEF where
  succ = genericSucc
  pred = genericPred

instance Bounded SCEF where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum SCEF where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson SCEF where
  encodeJson = encodeJsonEnumWith identity

instance DecodeJson SCEF where
  decodeJson = decodeJsonEnumWith identity

-- |PenaltyType
data PYTP
  = PYTP_A -- ^ Absolute
  | PYTP_N -- ^ Nominal rate
  | PYTP_I -- ^ Current interest rate differential
  | PYTP_O -- ^ No penalty

derive instance Generic PYTP _
derive instance Eq PYTP
derive instance Ord PYTP

instance Show PYTP where
  show = genericShow

instance Enum PYTP where
  succ = genericSucc
  pred = genericPred

instance Bounded PYTP where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum PYTP where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson PYTP where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "PYTP_")

instance DecodeJson PYTP where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "PYTP_")

-- |Option Type
data OPTP
  = OPTP_C -- ^ Call Option
  | OPTP_P -- ^ Put Option
  | OPTP_CP -- ^ Call-Put Option

derive instance Generic OPTP _
derive instance Eq OPTP
derive instance Ord OPTP

instance Show OPTP where
  show = genericShow

instance Enum OPTP where
  succ = genericSucc
  pred = genericPred

instance Bounded OPTP where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum OPTP where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson OPTP where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "OPTP_")

instance DecodeJson OPTP where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "OPTP_")

-- |Option Exercise Type
data OPXT
  = OPXT_E -- ^ European
  | OPXT_B -- ^ Bermudan
  | OPXT_A -- ^ American

derive instance Generic OPXT _
derive instance Eq OPXT
derive instance Ord OPXT

instance Show OPXT where
  show = genericShow

instance Enum OPXT where
  succ = genericSucc
  pred = genericPred

instance Bounded OPXT where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum OPXT where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson OPXT where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "OPXT_")

instance DecodeJson OPXT where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "OPXT_")

-- |Settlement
data DS
  = DS_S -- ^ Cash Settlement
  | DS_D -- ^ Physical Settlement

derive instance Generic DS _
derive instance Eq DS
derive instance Ord DS

instance Show DS where
  show = genericShow

instance Enum DS where
  succ = genericSucc
  pred = genericPred

instance Bounded DS where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum DS where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson DS where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "DS_")

instance DecodeJson DS where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "DS_")

-- |PrepaymentEffect
data PPEF
  = PPEF_N -- ^ No prepayment
  | PPEF_A -- ^ Prepayment allowed, prepayment results in reduction of PRNXT while MD remains
  | PPEF_M -- ^ Prepayment allowed, prepayment results in reduction of MD while PRNXT remains

derive instance Generic PPEF _
derive instance Eq PPEF
derive instance Ord PPEF

instance Show PPEF where
  show = genericShow

instance Enum PPEF where
  succ = genericSucc
  pred = genericPred

instance Bounded PPEF where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum PPEF where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

instance EncodeJson PPEF where
  encodeJson = encodeJsonEnumWith (tryStripPrefix $ Pattern "PPEF_")

instance DecodeJson PPEF where
  decodeJson = decodeJsonEnumWith (tryStripPrefix $ Pattern "PPEF_")

-- |CyclePeriod
data Period
  = P_D -- ^ Day
  | P_W -- ^ Week
  | P_M -- ^ Month
  | P_Q -- ^ Quarter
  | P_H -- ^ Half year
  | P_Y -- ^ Year

derive instance Generic Period _
derive instance Eq Period
derive instance Ord Period

instance Show Period where
  show = genericShow

instance Enum Period where
  succ = genericSucc
  pred = genericPred

instance Bounded Period where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Period where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

periodToString :: Period -> String
periodToString = tryStripPrefix (Pattern "P_") <<< show

periodFromString :: String -> Maybe Period
periodFromString = decodeEnumWith (tryStripPrefix (Pattern "P_"))

-- |CycleStub
data Stub
  = ShortStub -- ^ Short last stub
  | LongStub -- ^ Long last stub

derive instance Generic Stub _
derive instance Eq Stub
derive instance Ord Stub

instance Show Stub where
  show = genericShow

instance Enum Stub where
  succ = genericSucc
  pred = genericPred

instance Bounded Stub where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum Stub where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum

stubToString :: Stub -> String
stubToString ShortStub = "1"
stubToString LongStub = "0"

stubFromString :: String -> Maybe Stub
stubFromString = case _ of
  "1" -> Just ShortStub
  "0" -> Just LongStub
  _ -> Nothing

-- |Cycle
type Cycle =
  { n :: Int
  , p :: Period
  , stub :: Stub
  , includeEndDay :: Boolean
  }

encodeCycle :: Cycle -> String
encodeCycle { n, p, stub } =
  "P" <> show n <> periodToString p <> "L" <> stubToString stub

decodeCycle :: String -> Maybe Cycle
decodeCycle str = do
  let
    periods = String.joinWith "|" <<< map periodToString $ (upFromIncluding bottom :: Array Period)
    -- for example: "P1ML0
    pattern = "P([0-9]+)([" <> periods <> "])" <> "L" <> "([0|1])"
    regex = unsafeRegex pattern mempty
  m <- match regex str
  case NA.toArray m of
    [ _, Just n, Just p, Just s ] -> do
      { n: _, p: _, stub: _, includeEndDay: false }
        <$> Int.fromString n
        <*> periodFromString p
        <*> stubFromString s
    _ -> Nothing

-- For applicability failures
data TermValidationError
  = Required String
  | NotApplicable String

derive instance Eq TermValidationError

instance Show TermValidationError where
  show (Required s) = "Missing required term: " <> s
  show (NotApplicable s) = "Term not applicable to contract: " <> s

{-| ACTUS contract terms and attributes are defined in
    https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-terms.json
-}
type ContractTermsOptsRow r =
  ( -- General
    --   contractId :: String
    -- , contractType :: CT
    -- , contractRole :: CR
    settlementCurrency :: Maybe String

  -- Calendar
  , initialExchangeDate :: Maybe DateTime -- ^ Initial Exchange Date
  , dayCountConvention :: Maybe DCC -- ^ Day Count Convention
  -- , scheduleConfig :: ScheduleConfig

  -- -- Contract Identification
  -- , statusDate :: DateTime -- ^ Status Date
  , marketObjectCode :: Maybe String -- ^ Market Object Code

  -- -- Counterparty
  , contractPerformance :: Maybe PRF -- ^ Contract Performance
  , creditEventTypeCovered :: Maybe CETC -- ^ Credit Event Type Covered
  , coverageOfCreditEnhancement :: Maybe Decimal -- ^ Coverage Of Credit Enhancement
  , guaranteedExposure :: Maybe CEGE -- ^ Guaranteed Exposure

  -- -- Fees
  , cycleOfFee :: Maybe Cycle -- ^ Cycle Of Fee
  , cycleAnchorDateOfFee :: Maybe DateTime -- ^ Cycle Anchor Date Of Fee
  , feeAccrued :: Maybe Decimal -- ^ Fee Accrued
  , feeBasis :: Maybe FEB -- ^ Fee Basis
  , feeRate :: Maybe Decimal -- ^ Fee Rate

  -- Interest
  , cycleAnchorDateOfInterestPayment :: Maybe DateTime -- ^ Cycle Anchor Date Of Interest Payment
  , cycleOfInterestPayment :: Maybe Cycle -- ^ Cycle Of Interest Payment
  , accruedInterest :: Maybe Decimal -- ^ Accrued Interest
  , capitalizationEndDate :: Maybe DateTime -- ^ Capitalization End Date
  , cycleAnchorDateOfInterestCalculationBase :: Maybe DateTime -- ^ Cycle Anchor Date Of Interest Calculation Base
  , cycleOfInterestCalculationBase :: Maybe Cycle -- ^ Cycle Of Interest Calculation Base
  , interestCalculationBase :: Maybe IPCB -- ^ Interest Calculation Base
  , interestCalculationBaseAmount :: Maybe Decimal -- ^ Interest Calculation Base Amount
  , nominalInterestRate :: Maybe Decimal -- ^ Nominal Interest Rate
  , nominalInterestRate2 :: Maybe Decimal -- ^ Nominal Interest Rate (Second Leg in Plain Vanilla Swap)
  , interestScalingMultiplier :: Maybe Decimal -- ^ Interest Scaling Multiplier

  -- Dates
  , maturityDate :: Maybe DateTime -- ^ Maturity Date
  , amortizationDate :: Maybe DateTime -- ^ Amortization Date
  , exerciseDate :: Maybe DateTime -- ^ Exercise Date

  -- -- Notional Principal
  , notionalPrincipal :: Maybe Decimal -- ^ Notional Principal
  , premiumDiscountAtIED :: Maybe Decimal -- ^ Premium Discount At IED
  , cycleAnchorDateOfPrincipalRedemption :: Maybe DateTime -- ^ Cycle Anchor Date Of Principal Redemption
  , cycleOfPrincipalRedemption :: Maybe Cycle -- ^ Cycle Of Principal Redemption
  , nextPrincipalRedemptionPayment :: Maybe Decimal -- ^ Next Principal Redemption Payment
  , purchaseDate :: Maybe DateTime -- ^ Purchase Date
  , priceAtPurchaseDate :: Maybe Decimal -- ^ Price At Purchase Date
  , terminationDate :: Maybe DateTime -- ^ Termination Date
  , priceAtTerminationDate :: Maybe Decimal -- ^ Price At Termination Date
  , quantity :: Maybe Decimal -- ^ Quantity
  , currency :: Maybe String -- ^ The currency of the cash flows
  , currency2 :: Maybe String -- ^ The currency of the cash flows of the second leg

  -- Scaling Index
  , scalingIndexAtStatusDate :: Maybe Decimal -- ^ Scaling Index At Status Date
  , cycleAnchorDateOfScalingIndex :: Maybe DateTime -- ^ Cycle Anchor Date Of Scaling Index
  , cycleOfScalingIndex :: Maybe Cycle -- ^ Cycle Of Scaling Index
  , scalingEffect :: Maybe SCEF -- ^ Scaling Effect
  , scalingIndexAtContractDealDate :: Maybe Decimal -- ^ Scaling Index At Contract Deal Date
  , marketObjectCodeOfScalingIndex :: Maybe String -- ^ Market Object Code Of Scaling Index
  , notionalScalingMultiplier :: Maybe Decimal -- ^ Notional Scaling Multiplier

  -- Optionality
  , cycleOfOptionality :: Maybe Cycle -- ^ Cycle Of Optionality
  , cycleAnchorDateOfOptionality :: Maybe DateTime -- ^ Cycle Anchor Date Of Optionality
  , optionType :: Maybe OPTP -- ^ Option Type
  , optionStrike1 :: Maybe Decimal -- ^ Option Strike 1
  , optionExerciseType :: Maybe OPXT -- ^ Option Exercise Type

  -- Settlement
  , settlementPeriod :: Maybe Cycle -- ^ Settlement Period
  , deliverySettlement :: Maybe DS -- ^ Delivery Settlement
  , exerciseAmount :: Maybe Decimal -- ^ Exercise Amount
  , futuresPrice :: Maybe Decimal -- ^ Futures Price

  -- Penalty
  , penaltyRate :: Maybe Decimal -- ^ Penalty Rate
  , penaltyType :: Maybe PYTP -- ^ Penalty Type
  , prepaymentEffect :: Maybe PPEF -- ^ Prepayment Effect

  -- Rate Reset
  , cycleOfRateReset :: Maybe Cycle -- ^ Cycle Of Rate Reset
  , cycleAnchorDateOfRateReset :: Maybe DateTime -- ^ Cycle Anchor Date Of Rate Reset
  , nextResetRate :: Maybe Decimal -- ^ Next Reset Rate
  , rateSpread :: Maybe Decimal -- ^ Rate Spread
  , rateMultiplier :: Maybe Decimal -- ^ Rate Multiplier
  , periodFloor :: Maybe Decimal -- ^ Period Floor
  , periodCap :: Maybe Decimal -- ^ Period Cap
  , lifeCap :: Maybe Decimal -- ^ Life Cap
  , lifeFloor :: Maybe Decimal -- ^ Life Floor
  , marketObjectCodeOfRateReset :: Maybe String -- ^ Market Object Code Of Rate Reset

  -- Dividend
  , cycleOfDividendPayment :: Maybe Cycle -- ^ Cycle Of Dividend
  , cycleAnchorDateOfDividendPayment :: Maybe DateTime -- ^ Cycle Anchor Date Of Dividend
  , nextDividendPaymentAmount :: Maybe Decimal -- ^ Next Dividend Payment Amount
  | r
  )

type ContractTermsRow =
  ContractTermsOptsRow +
    ( -- General
      contractId :: String
    , contractType :: CT
    , contractRole :: CR

    -- Calendar
    , scheduleConfig :: ScheduleConfig

    -- Contract Identification
    , statusDate :: DateTime -- ^ Status Date
    )

newtype ContractTerms :: Type
newtype ContractTerms = ContractTerms { | ContractTermsRow }

derive instance Newtype ContractTerms _
derive instance Generic ContractTerms _
derive instance Eq ContractTerms
instance Show ContractTerms where
  show = genericShow

decodeDecimal :: Json -> Either JsonDecodeError Decimal
decodeDecimal = decodeFromString (String.trimStart >>> Decimal.fromString)

encodeDecimal :: Decimal -> Json
encodeDecimal = fromString <<< Decimal.toString

decodeDateTime :: Json -> Either JsonDecodeError DateTime
decodeDateTime json = do
  str <- decodeJson json
  let
    -- The function `parse` should be referentialy transparent
    -- if we enforced UTC time zone.
    jsDate = unsafePerformEffect $ JSDate.parse $ str <> "Z"
  note (UnexpectedValue json) $ JSDate.toDateTime jsDate

instance DecodeJson ContractTerms where
  decodeJson json = do
    ContractTerms <$> execRecordBuilderM json Ix.do

      -- General
      contractIdValue <- askField "contractID" <|> askField "contractId"
      contractId <- liftEither $ decodeJson contractIdValue
      insertProp (Proxy :: Proxy "contractId") contractId
      (Proxy :: Proxy "contractType") := decodeJson
      (Proxy :: Proxy "contractRole") :=! decodeJson $ CR_RPA
      (Proxy :: Proxy "settlementCurrency") :=? decodeJson

      -- Calendar
      (Proxy :: Proxy "initialExchangeDate") :=? decodeDateTime
      (Proxy :: Proxy "dayCountConvention") :=? decodeJson
      scheduleConfig <- Ix.do
        obj <- askObject
        liftEither $ execRecordBuilderM (fromObject obj) Ix.do
          (Proxy :: Proxy "calendar") :=?! decodeJson $ CLDR_NC
          (Proxy :: Proxy "endOfMonthConvention") :=?! decodeJson $ EOMC_SD
          (Proxy :: Proxy "businessDayConvention") :=?! decodeJson $ BDC_NULL
      insertProp (Proxy :: Proxy "scheduleConfig") scheduleConfig

      -- Contract Identification
      (Proxy :: Proxy "statusDate") := decodeDateTime
      (Proxy :: Proxy "marketObjectCode") :=? decodeJson

      -- Counterparty
      (Proxy :: Proxy "contractPerformance") :=? decodeJson
      (Proxy :: Proxy "creditEventTypeCovered") :=? decodeJson
      (Proxy :: Proxy "coverageOfCreditEnhancement") :=? decodeDecimal
      (Proxy :: Proxy "guaranteedExposure") :=? decodeJson

      -- Fees
      (Proxy :: Proxy "cycleOfFee") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "cycleAnchorDateOfFee") :=? decodeDateTime
      (Proxy :: Proxy "feeAccrued") :=? decodeDecimal
      (Proxy :: Proxy "feeBasis") :=? decodeJson
      (Proxy :: Proxy "feeRate") :=? decodeDecimal

      -- Interest
      (Proxy :: Proxy "cycleAnchorDateOfInterestPayment") :=? decodeDateTime
      (Proxy :: Proxy "cycleOfInterestPayment") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "accruedInterest") :=? decodeDecimal
      (Proxy :: Proxy "capitalizationEndDate") :=? decodeDateTime
      (Proxy :: Proxy "cycleAnchorDateOfInterestCalculationBase") :=? decodeDateTime
      (Proxy :: Proxy "cycleOfInterestCalculationBase") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "interestCalculationBase") :=? decodeJson
      (Proxy :: Proxy "interestCalculationBaseAmount") :=? decodeDecimal
      (Proxy :: Proxy "nominalInterestRate") :=? decodeDecimal
      (Proxy :: Proxy "nominalInterestRate2") :=? decodeDecimal
      (Proxy :: Proxy "interestScalingMultiplier") :=? decodeDecimal

      -- Dates
      (Proxy :: Proxy "maturityDate") :=? decodeDateTime
      (Proxy :: Proxy "amortizationDate") :=? decodeDateTime
      (Proxy :: Proxy "exerciseDate") :=? decodeDateTime

      -- Notional Principal
      (Proxy :: Proxy "notionalPrincipal") :=? decodeDecimal
      (Proxy :: Proxy "premiumDiscountAtIED") :=? decodeDecimal
      (Proxy :: Proxy "cycleAnchorDateOfPrincipalRedemption") :=? decodeDateTime
      (Proxy :: Proxy "cycleOfPrincipalRedemption") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "nextPrincipalRedemptionPayment") :=? decodeDecimal
      (Proxy :: Proxy "purchaseDate") :=? decodeDateTime
      (Proxy :: Proxy "priceAtPurchaseDate") :=? decodeDecimal
      (Proxy :: Proxy "terminationDate") :=? decodeDateTime
      (Proxy :: Proxy "priceAtTerminationDate") :=? decodeDecimal
      (Proxy :: Proxy "quantity") :=? decodeDecimal
      (Proxy :: Proxy "currency") :=? decodeJson
      (Proxy :: Proxy "currency2") :=? decodeJson

      -- Scaling Index
      (Proxy :: Proxy "scalingIndexAtStatusDate") :=? decodeDecimal
      (Proxy :: Proxy "cycleAnchorDateOfScalingIndex") :=? decodeDateTime
      (Proxy :: Proxy "cycleOfScalingIndex") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "scalingEffect") :=? decodeJson
      (Proxy :: Proxy "scalingIndexAtContractDealDate") :=? decodeDecimal
      (Proxy :: Proxy "marketObjectCodeOfScalingIndex") :=? decodeJson
      (Proxy :: Proxy "notionalScalingMultiplier") :=? decodeDecimal

      -- Optionality
      (Proxy :: Proxy "cycleOfOptionality") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "cycleAnchorDateOfOptionality") :=? decodeDateTime
      (Proxy :: Proxy "optionType") :=? decodeJson
      (Proxy :: Proxy "optionStrike1") :=? decodeDecimal
      (Proxy :: Proxy "optionExerciseType") :=? decodeJson

      -- Settlement
      (Proxy :: Proxy "settlementPeriod") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "deliverySettlement") :=? decodeJson
      (Proxy :: Proxy "exerciseAmount") :=? decodeDecimal
      (Proxy :: Proxy "futuresPrice") :=? decodeDecimal

      -- Penalty
      (Proxy :: Proxy "penaltyRate") :=? decodeDecimal
      (Proxy :: Proxy "penaltyType") :=? decodeJson
      (Proxy :: Proxy "prepaymentEffect") :=? decodeJson

      -- Rate Reset
      (Proxy :: Proxy "cycleOfRateReset") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "cycleAnchorDateOfRateReset") :=? decodeDateTime
      (Proxy :: Proxy "nextResetRate") :=? decodeDecimal
      (Proxy :: Proxy "rateSpread") :=? decodeDecimal
      (Proxy :: Proxy "rateMultiplier") :=? decodeDecimal
      (Proxy :: Proxy "periodFloor") :=? decodeDecimal
      (Proxy :: Proxy "periodCap") :=? decodeDecimal
      (Proxy :: Proxy "lifeCap") :=? decodeDecimal
      (Proxy :: Proxy "lifeFloor") :=? decodeDecimal
      (Proxy :: Proxy "marketObjectCodeOfRateReset") :=? decodeJson

      -- Dividend
      (Proxy :: Proxy "cycleOfDividendPayment") :=? decodeFromString decodeCycle
      (Proxy :: Proxy "cycleAnchorDateOfDividendPayment") :=? decodeDateTime
      (Proxy :: Proxy "nextDividendPaymentAmount") :=? decodeDecimal

mkContractTerms
  :: forall r r'
   . Row.Union r (ContractTermsOptsRow + ()) r'
  => Row.Nub r' ContractTermsRow
  => { | r }
  -> ContractTerms
mkContractTerms props = ContractTerms $ Record.merge props emptyOpts
  where
  emptyOpts :: { | ContractTermsOptsRow + () }
  emptyOpts =
    { -- General
      --  contractId :: String
      -- , contractType :: CT
      -- , contractRole :: CR
      settlementCurrency: Nothing :: Maybe String

    -- Calendar
    , initialExchangeDate: Nothing :: Maybe DateTime -- ^ Initial Exchange Date
    , dayCountConvention: Nothing :: Maybe DCC -- ^ Day Count Convention
    -- , scheduleConfig :: ScheduleConfig

    -- -- Contract Identification
    -- , statusDate :: DateTime -- ^ Status Date
    , marketObjectCode: Nothing :: Maybe String -- ^ Market Object Code

    -- -- Counterparty
    , contractPerformance: Nothing :: Maybe PRF -- ^ Contract Performance
    , creditEventTypeCovered: Nothing :: Maybe CETC -- ^ Credit Event Type Covered
    , coverageOfCreditEnhancement: Nothing :: Maybe Decimal -- ^ Coverage Of Credit Enhancement
    , guaranteedExposure: Nothing :: Maybe CEGE -- ^ Guaranteed Exposure

    -- -- Fees
    , cycleOfFee: Nothing :: Maybe Cycle -- ^ Cycle Of Fee
    , cycleAnchorDateOfFee: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Fee
    , feeAccrued: Nothing :: Maybe Decimal -- ^ Fee Accrued
    , feeBasis: Nothing :: Maybe FEB -- ^ Fee Basis
    , feeRate: Nothing :: Maybe Decimal -- ^ Fee Rate

    -- Interest
    , cycleAnchorDateOfInterestPayment: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Interest Payment
    , cycleOfInterestPayment: Nothing :: Maybe Cycle -- ^ Cycle Of Interest Payment
    , accruedInterest: Nothing :: Maybe Decimal -- ^ Accrued Interest
    , capitalizationEndDate: Nothing :: Maybe DateTime -- ^ Capitalization End Date
    , cycleAnchorDateOfInterestCalculationBase: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Interest Calculation Base
    , cycleOfInterestCalculationBase: Nothing :: Maybe Cycle -- ^ Cycle Of Interest Calculation Base
    , interestCalculationBase: Nothing :: Maybe IPCB -- ^ Interest Calculation Base
    , interestCalculationBaseAmount: Nothing :: Maybe Decimal -- ^ Interest Calculation Base Amount
    , nominalInterestRate: Nothing :: Maybe Decimal -- ^ Nominal Interest Rate
    , nominalInterestRate2: Nothing :: Maybe Decimal -- ^ Nominal Interest Rate (Second Leg in Plain Vanilla Swap)
    , interestScalingMultiplier: Nothing :: Maybe Decimal -- ^ Interest Scaling Multiplier

    -- Dates
    , maturityDate: Nothing :: Maybe DateTime -- ^ Maturity Date
    , amortizationDate: Nothing :: Maybe DateTime -- ^ Amortization Date
    , exerciseDate: Nothing :: Maybe DateTime -- ^ Exercise Date

    -- -- Notional Principal
    , notionalPrincipal: Nothing :: Maybe Decimal -- ^ Notional Principal
    , premiumDiscountAtIED: Nothing :: Maybe Decimal -- ^ Premium Discount At IED
    , cycleAnchorDateOfPrincipalRedemption: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Principal Redemption
    , cycleOfPrincipalRedemption: Nothing :: Maybe Cycle -- ^ Cycle Of Principal Redemption
    , nextPrincipalRedemptionPayment: Nothing :: Maybe Decimal -- ^ Next Principal Redemption Payment
    , purchaseDate: Nothing :: Maybe DateTime -- ^ Purchase Date
    , priceAtPurchaseDate: Nothing :: Maybe Decimal -- ^ Price At Purchase Date
    , terminationDate: Nothing :: Maybe DateTime -- ^ Termination Date
    , priceAtTerminationDate: Nothing :: Maybe Decimal -- ^ Price At Termination Date
    , quantity: Nothing :: Maybe Decimal -- ^ Quantity
    , currency: Nothing :: Maybe String -- ^ The currency of the cash flows
    , currency2: Nothing :: Maybe String -- ^ The currency of the cash flows of the second leg

    -- Scaling Index
    , scalingIndexAtStatusDate: Nothing :: Maybe Decimal -- ^ Scaling Index At Status Date
    , cycleAnchorDateOfScalingIndex: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Scaling Index
    , cycleOfScalingIndex: Nothing :: Maybe Cycle -- ^ Cycle Of Scaling Index
    , scalingEffect: Nothing :: Maybe SCEF -- ^ Scaling Effect
    , scalingIndexAtContractDealDate: Nothing :: Maybe Decimal -- ^ Scaling Index At Contract Deal Date
    , marketObjectCodeOfScalingIndex: Nothing :: Maybe String -- ^ Market Object Code Of Scaling Index
    , notionalScalingMultiplier: Nothing :: Maybe Decimal -- ^ Notional Scaling Multiplier

    -- Optionality
    , cycleOfOptionality: Nothing :: Maybe Cycle -- ^ Cycle Of Optionality
    , cycleAnchorDateOfOptionality: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Optionality
    , optionType: Nothing :: Maybe OPTP -- ^ Option Type
    , optionStrike1: Nothing :: Maybe Decimal -- ^ Option Strike 1
    , optionExerciseType: Nothing :: Maybe OPXT -- ^ Option Exercise Type

    -- Settlement
    , settlementPeriod: Nothing :: Maybe Cycle -- ^ Settlement Period
    , deliverySettlement: Nothing :: Maybe DS -- ^ Delivery Settlement
    , exerciseAmount: Nothing :: Maybe Decimal -- ^ Exercise Amount
    , futuresPrice: Nothing :: Maybe Decimal -- ^ Futures Price

    -- Penalty
    , penaltyRate: Nothing :: Maybe Decimal -- ^ Penalty Rate
    , penaltyType: Nothing :: Maybe PYTP -- ^ Penalty Type
    , prepaymentEffect: Nothing :: Maybe PPEF -- ^ Prepayment Effect

    -- Rate Reset
    , cycleOfRateReset: Nothing :: Maybe Cycle -- ^ Cycle Of Rate Reset
    , cycleAnchorDateOfRateReset: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Rate Reset
    , nextResetRate: Nothing :: Maybe Decimal -- ^ Next Reset Rate
    , rateSpread: Nothing :: Maybe Decimal -- ^ Rate Spread
    , rateMultiplier: Nothing :: Maybe Decimal -- ^ Rate Multiplier
    , periodFloor: Nothing :: Maybe Decimal -- ^ Period Floor
    , periodCap: Nothing :: Maybe Decimal -- ^ Period Cap
    , lifeCap: Nothing :: Maybe Decimal -- ^ Life Cap
    , lifeFloor: Nothing :: Maybe Decimal -- ^ Life Floor
    , marketObjectCodeOfRateReset: Nothing :: Maybe String -- ^ Market Object Code Of Rate Reset

    -- Dividend
    , cycleOfDividendPayment: Nothing :: Maybe Cycle -- ^ Cycle Of Dividend
    , cycleAnchorDateOfDividendPayment: Nothing :: Maybe DateTime -- ^ Cycle Anchor Date Of Dividend
    , nextDividendPaymentAmount: Nothing :: Maybe Decimal -- ^ Next Dividend Payment Amount
    }
