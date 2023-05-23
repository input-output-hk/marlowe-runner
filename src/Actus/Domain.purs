module Actus.Domain
  ( module Actus.Domain.BusinessEvents
  , module Actus.Domain.ContractState
  , module Actus.Domain.ContractTerms
  , module Actus.Domain.Schedule
  , _abs
  , _max
  , _min
  , _fromDecimal
  , class ActusOps
  , CashFlow(..)
  , RiskFactors(..)
  , setDefaultContractTermValues
  , sign
  , marloweFixedPoint
  , Value'(..)
  , Observation'(..)
  ) where

import Prelude

import Actus.Domain.BusinessEvents (EventType(..))
import Actus.Domain.ContractState (ContractState(..))
import Actus.Domain.ContractTerms (BDC(..), CEGE(..), CETC(..), CR(..), CT(..), Calendar(..), ContractTerms(..), Cycle, DCC(..), DS(..), EOMC(..), FEB(..), IPCB(..), OPTP(..), OPXT(..), PPEF(..), PRF(..), PYTP(..), Period(..), SCEF(..), ScheduleConfig, Stub(..))
import Actus.Domain.Schedule (ShiftedDay, ShiftedSchedule, mkShiftedDay)
import Control.Alt ((<|>))
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal, fromNumber)
import Data.Decimal as Decimal
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Language.Marlowe.Core.V1.Semantics.Types (ValueId)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

class ActusOps a where
  _min :: a -> a -> a
  _max :: a -> a -> a
  _abs :: a -> a
  _fromDecimal :: Decimal -> a

instance ActusOps Decimal where
  _min = min
  _max = max
  _abs = Decimal.abs
  _fromDecimal x = x

data Value'
  = Constant' BigInt
  | NegValue' Value'
  | AddValue' Value' Value'
  | SubValue' Value' Value'
  | MulValue' Value' Value'
  | DivValue' Value' Value'
  | Cond' Observation' Value' Value'
  | UseValue' ValueId

data Observation'
  = AndObs' Observation' Observation'
  | OrObs' Observation' Observation'
  | NotObs' Observation'
  | ValueGE' Value' Value'
  | ValueGT' Value' Value'
  | ValueLT' Value' Value'
  | ValueLE' Value' Value'
  | ValueEQ' Value' Value'
  | TrueObs'
  | FalseObs'

instance Semiring Value' where
  add x y = AddValue' x y
  mul x y = DivValue' (MulValue' x y) (Constant' $ BigInt.fromInt marloweFixedPoint)
  one = Constant' $ BigInt.fromInt marloweFixedPoint
  zero = Constant' (BigInt.fromInt 0)

instance Ring Value' where
  sub x y = SubValue' x y

instance CommutativeRing Value'

instance EuclideanRing Value' where
  degree _ = 1
  div x y = DivValue' (MulValue' (Constant' $ BigInt.fromInt marloweFixedPoint) x) y -- TODO: different rounding, don't use DivValue
  mod _ _ = unsafeCrashWith "Partial implementation of EuclideanRing for Value'" -- TODO: complete implemenation

instance ActusOps Value' where
  _min x y = Cond' (ValueLT' x y) x y
  _max x y = Cond' (ValueGT' x y) x y
  _abs a = _max a (NegValue' a)
  _fromDecimal n = Constant' $ toMarloweFixedPoint n
    where
    toMarloweFixedPoint :: Decimal -> BigInt
    toMarloweFixedPoint i = unsafePartial (fromJust <<< BigInt.fromString <<< Decimal.toString <<< Decimal.floor $ (Decimal.fromInt marloweFixedPoint) * i)

derive instance Generic Value' _
derive instance Generic Observation' _

marloweFixedPoint :: Int
marloweFixedPoint = 1000000

-- | Risk factor observer
data RiskFactors a = RiskFactors
  { o_rf_CURS :: a
  , o_rf_RRMO :: a
  , o_rf_SCMO :: a
  , pp_payoff :: a
  }

-- | Cash flows
newtype CashFlow a b = CashFlow
  { contractId :: String
  , party :: b
  , counterparty :: b
  , paymentDay :: DateTime
  , calculationDay :: DateTime
  , event :: EventType
  , contractRole :: CR
  , amount :: a
  , notional :: a
  , currency :: String
  }

derive instance Generic (CashFlow a b) _
derive instance Newtype (CashFlow a b) _
instance (Show a, Show b) => Show (CashFlow a b) where
  show = genericShow

sign :: forall a. Ring a => CR -> a
sign CR_RPA = one
sign CR_RPL = negate one
sign CR_CLO = one
sign CR_CNO = one
sign CR_COL = one
sign CR_LG = one
sign CR_ST = negate one
sign CR_BUY = one
sign CR_SEL = negate one
sign CR_RFL = one
sign CR_PFL = negate one
sign CR_RF = one
sign CR_PF = negate one

setDefaultContractTermValues :: ContractTerms -> ContractTerms
setDefaultContractTermValues (ContractTerms ct) = ContractTerms $
  ct
    { scheduleConfig =
        { endOfMonthConvention: applyDefault EOMC_SD ct.scheduleConfig.endOfMonthConvention
        , businessDayConvention: applyDefault BDC_NULL ct.scheduleConfig.businessDayConvention
        , calendar: applyDefault CLDR_NC ct.scheduleConfig.calendar
        }
    , contractPerformance = applyDefault PRF_PF ct.contractPerformance
    , interestCalculationBase = applyDefault IPCB_NT ct.interestCalculationBase
    , premiumDiscountAtIED = applyDefault (fromNumber 0.0) ct.premiumDiscountAtIED
    , scalingEffect = applyDefault SE_OOO ct.scalingEffect
    , penaltyRate = applyDefault (fromNumber 0.0) ct.penaltyRate
    , penaltyType = applyDefault PYTP_O ct.penaltyType
    , prepaymentEffect = applyDefault PPEF_N ct.prepaymentEffect
    , rateSpread = applyDefault (fromNumber 0.0) ct.rateSpread
    , rateMultiplier = applyDefault (fromNumber 1.0) ct.rateMultiplier
    , feeAccrued = applyDefault (fromNumber 0.0) ct.feeAccrued
    , feeRate = applyDefault (fromNumber 0.0) ct.feeRate
    , accruedInterest = applyDefault (fromNumber 0.0) ct.accruedInterest
    , nominalInterestRate = applyDefault (fromNumber 0.0) ct.nominalInterestRate
    , priceAtPurchaseDate = applyDefault (fromNumber 0.0) ct.priceAtPurchaseDate
    , priceAtTerminationDate = applyDefault (fromNumber 0.0) ct.priceAtTerminationDate
    , scalingIndexAtContractDealDate = applyDefault (fromNumber 0.0) ct.scalingIndexAtContractDealDate
    , periodFloor = applyDefault (-infinity) ct.periodFloor
    , periodCap = applyDefault infinity ct.periodCap
    , lifeCap = applyDefault infinity ct.lifeCap
    , lifeFloor = applyDefault (-infinity) ct.lifeFloor
    }
  where
  infinity :: Decimal
  infinity = fromNumber $ 1.0 / 0.0

  applyDefault :: forall a. a -> Maybe a -> Maybe a
  applyDefault v o = o <|> Just v
