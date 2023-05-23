module Actus.Domain.ContractState where

import Actus.Domain.ContractTerms (PRF)
import Contrib.Data.Lens.Generic.Record (Lens'', mkNewtyped1Lenses)
import Data.DateTime (DateTime)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (class Strong)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

{-| ACTUS contract states are defined in
    https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-states.json
-}
newtype ContractState a = ContractState
  { tmd :: Maybe DateTime -- ^ Maturity Date (MD): The timestamp as per which the contract matures according to the initial terms or as per unscheduled events
  , nt :: a -- ^ Notional Principal (NT): The outstanding nominal value
  , ipnr :: a -- ^ Nominal Interest Rate (IPNR) : The applicable nominal rate
  , ipac :: a -- ^ Accrued Interest (IPAC): The current value of accrued interest
  , ipla :: Maybe a -- ^ Last Interst Period
  , feac :: a -- ^ Fee Accrued (FEAC): The current value of accrued fees
  , nsc :: a -- ^ Notional Scaling Multiplier (SCNT): The multiplier being applied to principal cash flows
  , isc :: a -- ^ InterestScalingMultiplier (SCIP): The multiplier being applied to interest cash flows
  , prf :: PRF -- ^ Contract Performance (PRF)
  , sd :: DateTime -- ^ Status Date (MD): The timestamp as per which the state is captured at any point in time
  , prnxt :: a -- ^ Next Principal Redemption Payment (PRNXT): The value at which principal is being repaid
  , ipcb :: a -- ^ Interest Calculation Base (IPCB)
  }

derive instance Newtype (ContractState a) _

_ContractState
  :: forall a p
   . Strong p
  => { feac :: Lens'' p (ContractState a) a
     , ipac :: Lens'' p (ContractState a) a
     , ipcb :: Lens'' p (ContractState a) a
     , ipla :: Lens'' p (ContractState a) (Maybe a)
     , ipnr :: Lens'' p (ContractState a) a
     , isc :: Lens'' p (ContractState a) a
     , nsc :: Lens'' p (ContractState a) a
     , nt :: Lens'' p (ContractState a) a
     , prf :: Lens'' p (ContractState a) PRF
     , prnxt :: Lens'' p (ContractState a) a
     , sd :: Lens'' p (ContractState a) DateTime
     , tmd :: Lens'' p (ContractState a) (Maybe DateTime)
     }
_ContractState = mkNewtyped1Lenses (Proxy :: Proxy ContractState)

statusDate = prop (Proxy :: Proxy "sd")
notionalPrincipal = prop (Proxy :: Proxy "nt")
nominalInterest = prop (Proxy :: Proxy "ipnr")
accruedInterest = prop (Proxy :: Proxy "ipac")
lastInterestPeriod = prop (Proxy :: Proxy "ipla")
interestCalculationBase = prop (Proxy :: Proxy "ipcb")
accruedFees = prop (Proxy :: Proxy "feac")
notionalScalingMultiplier = prop (Proxy :: Proxy "nsc")
interestScalingMultiplier = prop (Proxy :: Proxy "isc")
nextPrincipalRedemptionPayment = prop (Proxy :: Proxy "prnxt")
exerciseAmount = prop (Proxy :: Proxy "xa")
