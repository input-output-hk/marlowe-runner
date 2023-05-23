{-| = ACTUS state transformation functions -}
module Actus.Model.StateTransition
  ( CtxSTF(..)
  , stateTransition
  ) where

import Prelude

import Actus.Domain (class ActusOps, CT(..), ContractState(..), ContractTerms(..), EventType(..), FEB(..), IPCB(..), RiskFactors(..), SCEF(..), _abs, _fromDecimal, _max, _min, sign)
import Actus.Domain.ContractState as L
import Actus.Utility (annuity, inf, sup)
import Actus.Utility.YearFraction (yearFraction)
import Control.Monad.Reader (Reader, asks)
import Data.DateTime (DateTime)
import Data.Lens ((+~), (-~), (.~), (^.))
import Data.List (List(..), filter, singleton, tail, zipWith, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)

-- |The context for state transitions provides the contract terms in addition with
-- schedules and the maturity of the contract. Furthermore a function to retrieve
-- risk factors is available.
type CtxSTF a =
  { -- | Contract terms
    contractTerms :: ContractTerms
  ,
    -- | Fee payment schedule
    fpSchedule :: List DateTime
  ,
    -- | Principal redemption schedule
    prSchedule :: List DateTime
  ,
    -- | Interest payment schedule
    ipSchedule :: List DateTime
  ,
    -- | Maturity
    maturity :: Maybe DateTime
  ,
    -- | Riskfactors per event and time
    riskFactors :: EventType -> DateTime -> RiskFactors a
  }

-- |A state transition updates the contract state based on the type of event and the time.
-- `CtxSTF` provides in particular the contract terms and risk factors.
stateTransition
  :: forall a
   . EuclideanRing a
  => ActusOps a
  =>
  -- | Event type
  EventType
  ->
  -- | Time
  DateTime
  ->
  -- | Contract state
  ContractState a
  ->
  -- | Updated contract state
  Reader (CtxSTF a) (ContractState a)
stateTransition ev t sn = asks stateTransition'
  where
  stateTransition'
    { riskFactors
    , contractTerms
    , fpSchedule
    , prSchedule
    , maturity
    } = stf ev (riskFactors ev t) contractTerms sn
    where
    stf AD _ ct st = _STF_AD_ALL ct st t
    stf IED _ ct@(ContractTerms { contractType: PAM }) st = _STF_IED_PAM ct st t
    stf IED _ ct@(ContractTerms { contractType: LAM }) st = _STF_IED_LAM ct st t
    stf IED _ ct@(ContractTerms { contractType: NAM }) st = _STF_IED_LAM ct st t
    stf IED _ ct@(ContractTerms { contractType: ANN }) st = _STF_IED_LAM ct st t
    stf PR _ ct@(ContractTerms { contractType: LAM }) st = _STF_PR_LAM ct st t
    stf PR _ ct@(ContractTerms { contractType: NAM }) st = _STF_PR_NAM ct st t
    stf PR _ ct@(ContractTerms { contractType: ANN }) st = _STF_PR_NAM ct st t
    stf MD _ ct st = _STF_MD_ALL ct st t
    stf PP rf ct@(ContractTerms { contractType: PAM }) st = _STF_PP_PAM fs rf ct st t
    stf PP rf ct@(ContractTerms { contractType: LAM }) st = _STF_PP_LAM fs rf ct st t
    stf PP rf ct@(ContractTerms { contractType: NAM }) st = _STF_PP_LAM fs rf ct st t
    stf PP rf ct@(ContractTerms { contractType: ANN }) st = _STF_PP_LAM fs rf ct st t
    stf PY _ ct@(ContractTerms { contractType: PAM }) st = _STF_PY_PAM fs ct st t
    stf PY _ ct@(ContractTerms { contractType: LAM }) st = _STF_PY_LAM fs ct st t
    stf PY _ ct@(ContractTerms { contractType: NAM }) st = _STF_PY_LAM fs ct st t
    stf PY _ ct@(ContractTerms { contractType: ANN }) st = _STF_PY_LAM fs ct st t
    stf FP _ ct@(ContractTerms { contractType: PAM }) st = _STF_FP_PAM ct st t
    stf FP _ ct@(ContractTerms { contractType: LAM }) st = _STF_FP_LAM ct st t
    stf FP _ ct@(ContractTerms { contractType: NAM }) st = _STF_FP_LAM ct st t
    stf FP _ ct@(ContractTerms { contractType: ANN }) st = _STF_FP_LAM ct st t
    stf PRD _ ct@(ContractTerms { contractType: PAM }) st = _STF_PY_PAM fs ct st t
    stf PRD _ ct@(ContractTerms { contractType: LAM }) st = _STF_PY_PAM fs ct st t
    stf PRD _ ct@(ContractTerms { contractType: NAM }) st = _STF_PY_PAM fs ct st t
    stf PRD _ ct@(ContractTerms { contractType: ANN }) st = _STF_PY_PAM fs ct st t
    stf TD _ _ st = _STF_TD_ALL st t
    stf IP _ ct st = _STF_IP_PAM ct st t
    stf IPCI _ ct@(ContractTerms { contractType: PAM }) st = _STF_IPCI_PAM ct st t
    stf IPCI _ ct@(ContractTerms { contractType: LAM }) st = _STF_IPCI_LAM ct st t
    stf IPCI _ ct@(ContractTerms { contractType: NAM }) st = _STF_IPCI_LAM ct st t
    stf IPCI _ ct@(ContractTerms { contractType: ANN }) st = _STF_IPCI_LAM ct st t
    stf IPCB _ ct@(ContractTerms { contractType: LAM }) st = _STF_IPCB_LAM fs ct st t
    stf IPCB _ ct@(ContractTerms { contractType: NAM }) st = _STF_IPCB_LAM fs ct st t
    stf IPCB _ ct@(ContractTerms { contractType: ANN }) st = _STF_IPCB_LAM fs ct st t
    stf RR rf ct@(ContractTerms { contractType: PAM }) st = _STF_RR_PAM fs rf ct st t
    stf RR rf ct@(ContractTerms { contractType: LAM }) st = _STF_RR_LAM fs rf ct st t
    stf RR rf ct@(ContractTerms { contractType: NAM }) st = _STF_RR_LAM fs rf ct st t
    stf RR rf ct@(ContractTerms { contractType: ANN }) st = _STF_RR_ANN ps fs rf ct st t
    stf RRF rf ct@(ContractTerms { contractType: PAM }) st = _STF_RRF_PAM fs rf ct st t
    stf RRF rf ct@(ContractTerms { contractType: LAM }) st = _STF_RRF_LAM fs rf ct st t
    stf RRF rf ct@(ContractTerms { contractType: NAM }) st = _STF_RRF_LAM fs rf ct st t
    stf RRF rf ct@(ContractTerms { contractType: ANN }) st = _STF_RRF_ANN ps fs rf ct st t
    stf PRF _ ct@(ContractTerms { contractType: ANN }) st = _STF_PRF_ANN ps fs ct st t
    stf SC rf ct@(ContractTerms { contractType: PAM }) st = _STF_SC_PAM fs rf ct st t
    stf SC rf ct@(ContractTerms { contractType: LAM }) st = _STF_SC_LAM fs rf ct st t
    stf SC rf ct@(ContractTerms { contractType: NAM }) st = _STF_SC_LAM fs rf ct st t
    stf SC rf ct@(ContractTerms { contractType: ANN }) st = _STF_SC_LAM fs rf ct st t

    -----------------------
    -- Credit Event (CE) --
    -----------------------
    stf CE rf ct st = stf AD rf ct st
    -------------
    -- Default --
    -------------
    stf _ _ _ _ = sn

    fs =
      FeeSchedule
        { latestFeePayment: (fromMaybe t (sup fpSchedule t))
        , nextFeePayment: (fromMaybe t (inf fpSchedule t))
        }

    ps =
      PrincipalRedemptionSchedule
        { laterPrincipalRedemptionDates:
            ( let
                principalRedemptionDates = prSchedule ++ (maybeToList maturity)
                ContractState sn' = sn
              in
                filter (_ > sn'.sd) principalRedemptionDates
            )
        , nextPrincipalRedemption: (fromMaybe t (inf prSchedule t))
        }

append' :: forall a. List a -> List a -> List a
append' Nil ys = ys
append' (x : xs) ys = x : (append' xs ys)

infixl 8 append' as ++

maybeToList :: forall a. Maybe a -> List a
maybeToList (Just x) = singleton x
maybeToList Nothing = mempty

data FeeSchedule = FeeSchedule
  { latestFeePayment :: DateTime
  , nextFeePayment :: DateTime
  }

data PrincipalRedemptionSchedule = PrincipalRedemptionSchedule
  { laterPrincipalRedemptionDates :: List DateTime
  , nextPrincipalRedemption :: DateTime
  }

thisOr0 :: forall a. Ring a => Maybe a -> a
thisOr0 = fromMaybe zero

---------------------
-- Monitoring (AD) --
---------------------

_STF_AD_ALL :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_AD_ALL
  ( ContractTerms
      { dayCountConvention: Just dcc
      , maturityDate
      }
  )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
  in
    ContractState $
      s # L.statusDate .~ t
        # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
_STF_AD_ALL _ s _ = s

----------------------------
-- Initial Exchange (IED) --
----------------------------

_STF_IED_PAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_IED_PAM
  ( ContractTerms
      { nominalInterestRate
      , notionalPrincipal: Just nt
      , accruedInterest: Just ipac
      , contractRole
      }
  )
  (ContractState s)
  t = ContractState $
  s # L.notionalPrincipal .~ sign contractRole * _fromDecimal nt
    # L.nominalInterest .~ thisOr0 (_fromDecimal <$> nominalInterestRate)
    # L.accruedInterest .~ _fromDecimal ipac
    # L.statusDate .~ t
_STF_IED_PAM
  ( ContractTerms
      { nominalInterestRate: Just ipnr
      , notionalPrincipal: Just nt
      , cycleAnchorDateOfInterestPayment: Just ipanx
      , dayCountConvention: Just dcc
      , contractRole
      , maturityDate
      }
  )
  (ContractState s)
  t =
  let
    nt' = sign contractRole * _fromDecimal nt
    timeFromInterestPaymentAnchorToNow = _fromDecimal $ yearFraction dcc ipanx t maturityDate
    timeFromInterestPaymentAnchorToNow' = _fromDecimal $ yearFraction dcc ipanx t Nothing
    ipnr' = _fromDecimal ipnr
  in
    ContractState $
      s # L.notionalPrincipal .~ nt'
        # L.nominalInterest .~ ipnr'
        # L.accruedInterest .~ timeFromInterestPaymentAnchorToNow' * timeFromInterestPaymentAnchorToNow * nt' * ipnr' -- TODO: correct?
        # L.statusDate .~ t
_STF_IED_PAM
  ( ContractTerms
      { nominalInterestRate: Nothing
      , notionalPrincipal: Just nt
      , contractRole
      }
  )
  (ContractState s)
  t = ContractState $
  s # L.notionalPrincipal .~ sign contractRole * _fromDecimal nt
    # L.accruedInterest .~ zero
    # L.statusDate .~ t
_STF_IED_PAM _ s _ = s

_STF_IED_LAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_IED_LAM
  ct@
    ( ContractTerms
        { notionalPrincipal: Just nt
        , nominalInterestRate: Just ipnr
        , dayCountConvention: Just dcc
        , maturityDate
        , contractRole
        }
    )
  (ContractState s)
  t =
  let
    nt' = sign contractRole * _fromDecimal nt
    ipcb' = interestCalculationBase' ct
      where
      interestCalculationBase' (ContractTerms { interestCalculationBase: Just IPCB_NT }) = nt'
      interestCalculationBase' (ContractTerms { interestCalculationBaseAmount: Just ipcba }) = sign contractRole * (_fromDecimal ipcba)
      interestCalculationBase' _ = zero
    ipac' = interestAccrued' ct
      where
      interestAccrued' (ContractTerms { accruedInterest: Just ipac }) = sign contractRole * (_fromDecimal ipac)
      interestAccrued' (ContractTerms { cycleAnchorDateOfInterestPayment: Just ipanx })
        | ipanx < t =
            let
              timeFromInterestPaymentAnchorToNow = _fromDecimal $ yearFraction dcc ipanx t maturityDate
            in
              timeFromInterestPaymentAnchorToNow * nt' * ipcb'
      interestAccrued' _ = zero
  in
    ContractState $
      s # L.notionalPrincipal .~ nt'
        # L.nominalInterest .~ _fromDecimal ipnr
        # L.accruedInterest .~ ipac'
        # L.interestCalculationBase .~ ipcb'
        # L.statusDate .~ t
_STF_IED_LAM _ s _ = s

-------------------------------
-- Principal Redemption (PR) --
-------------------------------

_STF_PR_LAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PR_LAM
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , feeRate
        , contractRole
        , maturityDate
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    nt' = (s ^. L.notionalPrincipal)
      - sign contractRole *
          ( (s ^. L.nextPrincipalRedemptionPayment)
              - sign contractRole * _max zero (_abs (s ^. L.nextPrincipalRedemptionPayment) - _abs (s ^. L.notionalPrincipal))
          )
    ipcb' = interestCalculationBase' ct
      where
      interestCalculationBase' (ContractTerms { interestCalculationBase: Just IPCB_NTL }) = s ^. L.interestCalculationBase
      interestCalculationBase' _ = nt'
  in
    ContractState $
      s # L.notionalPrincipal .~ nt'
        # L.accruedFees +~ timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (_fromDecimal <$> feeRate)
        # L.interestCalculationBase .~ ipcb'
        # L.accruedInterest +~ (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase) * timeFromLastEvent
        # L.statusDate .~ t
_STF_PR_LAM _ s _ = s

_STF_PR_NAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PR_NAM
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , feeRate
        , contractRole
        , maturityDate
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    ipac' = (s ^. L.accruedInterest) + (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase) * timeFromLastEvent
    nt' = (s ^. L.notionalPrincipal) - sign contractRole * r
      where
      r = ra - _max zero (ra - _abs (s ^. L.notionalPrincipal))
      ra = (s ^. L.nextPrincipalRedemptionPayment) - sign contractRole * ipac'
    ipcb' = interestCalculationBase' ct
      where
      interestCalculationBase' (ContractTerms { interestCalculationBase: Just IPCB_NT }) = nt'
      interestCalculationBase' _ = s ^. L.interestCalculationBase
  in
    ContractState $
      s # L.notionalPrincipal .~ nt'
        # L.accruedFees +~ timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (_fromDecimal <$> feeRate)
        # L.interestCalculationBase .~ ipcb'
        # L.accruedInterest .~ ipac'
        # L.statusDate .~ t
_STF_PR_NAM _ s _ = s

-------------------
-- Maturity (MD) --
-------------------

_STF_MD_ALL :: forall a. Ring a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_MD_ALL _ (ContractState s) t = ContractState $
  s # L.notionalPrincipal .~ zero
    # L.accruedInterest .~ zero
    # L.accruedFees .~ zero
    # L.statusDate .~ t

-------------------------------
-- Principal Prepayment (PP) --
-------------------------------

_STF_PP_PAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PP_PAM
  fs
  ( RiskFactors
      { pp_payoff
      }
  )
  ct
  s
  t =
  let
    (ContractState s') = _STF_PY_PAM fs ct s t
  in
    ContractState $ s' # L.notionalPrincipal -~ pp_payoff

_STF_PP_LAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PP_LAM
  fs
  ( RiskFactors
      { pp_payoff
      }
  )
  ct@
    ( ContractTerms
        { interestCalculationBase: Just IPCB_NT
        }
    )
  s'@(ContractState s)
  t =
  let
    (ContractState s'') = _STF_PY_PAM fs ct s' t
  in
    ContractState $
      s'' # L.notionalPrincipal .~ (s ^. L.notionalPrincipal) - pp_payoff
        # L.interestCalculationBase .~ (s ^. L.notionalPrincipal)
_STF_PP_LAM
  fs
  ( RiskFactors
      { pp_payoff
      }
  )
  ct
  s'@(ContractState s)
  t =
  let
    (ContractState s'') = _STF_PY_PAM fs ct s' t
  in
    ContractState $ s'' # L.notionalPrincipal .~ (s ^. L.notionalPrincipal) - pp_payoff
      # L.interestCalculationBase .~ (s ^. L.interestCalculationBase)

--------------------------
-- Penalty Payment (PY) --
--------------------------

_STF_PY_PAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PY_PAM
  _
  ( ContractTerms
      { dayCountConvention: Just dcc
      , notionalPrincipal: Just nt'
      , maturityDate
      , feeBasis: Just FEB_N
      , feeRate: Just fer
      }
  )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
  in
    ContractState $
      s # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
        # L.accruedFees +~ timeFromLastEvent * (_fromDecimal fer) * (_fromDecimal nt')
        # L.statusDate .~ t
_STF_PY_PAM
  (FeeSchedule fs)
  ( ContractTerms
      { dayCountConvention: Just dcc
      , maturityDate
      , contractRole
      , feeRate: Just fer
      }
  )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    timeFromLatestFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment t maturityDate
    timeFromLatestToNextFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment fs.nextFeePayment maturityDate
  in
    ContractState $
      s # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
        # L.accruedFees .~ _max zero (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * (_fromDecimal fer)
        # L.statusDate .~ t
_STF_PY_PAM _ _ s _ = s

_STF_PY_LAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PY_LAM
  (FeeSchedule fs)
  ct@
    ( ContractTerms
        { feeRate: Just fer
        , dayCountConvention: Just dcc
        , maturityDate
        , contractRole
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    feac' = feeAccrued' ct
      where
      feeAccrued' (ContractTerms { feeBasis: Just FEB_N }) = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * (_fromDecimal fer)
      feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * (_fromDecimal fer)
        where
        timeFromLatestFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment t maturityDate
        timeFromLatestToNextFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment fs.nextFeePayment maturityDate
  in
    ContractState $
      s # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
        # L.accruedFees .~ feac'
        # L.statusDate .~ t
_STF_PY_LAM
  _
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , maturityDate
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    feac' = feeAccrued' ct
      where
      feeAccrued' (ContractTerms { feeBasis: Just FEB_N }) = s ^. L.accruedFees
      feeAccrued' _ = zero
  in
    ContractState $
      s # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
        # L.accruedFees .~ feac'
        # L.statusDate .~ t
_STF_PY_LAM _ _ s _ = s

----------------------
-- Fee Payment (FP) --
----------------------

_STF_FP_PAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_FP_PAM
  ( ContractTerms
      { dayCountConvention: Just dcc
      , maturityDate
      }
  )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
  in
    ContractState $
      s # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
        # L.accruedFees .~ zero
        # L.statusDate .~ t
_STF_FP_PAM _ s _ = s

_STF_FP_LAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_FP_LAM
  ( ContractTerms
      { dayCountConvention: Just dcc
      , maturityDate
      }
  )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
  in
    ContractState $
      s # L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
        # L.accruedFees .~ zero
        # L.statusDate .~ t
_STF_FP_LAM _ s _ = s

----------------------
-- Termination (TD) --
----------------------

_STF_TD_ALL :: forall a. Ring a => ContractState a -> DateTime -> ContractState a
_STF_TD_ALL
  (ContractState s)
  t = ContractState $
  s # L.notionalPrincipal .~ zero
    # L.accruedInterest .~ zero
    # L.accruedFees .~ zero
    # L.nominalInterest .~ zero
    # L.statusDate .~ t

---------------------------
-- Interest Payment (IP) --
---------------------------

_STF_IP_PAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_IP_PAM
  ( ContractTerms
      { dayCountConvention: Just dcc
      , feeRate
      , maturityDate
      }
  )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
  in
    ContractState $
      s # L.accruedInterest .~ zero
        # L.accruedFees .~ timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (_fromDecimal <$> feeRate)
        # L.statusDate .~ t
_STF_IP_PAM _ s _ = s

------------------------------------
-- Interest Capitalization (IPCI) --
------------------------------------

_STF_IPCI_PAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_IPCI_PAM
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , maturityDate
        }
    )
  s'@(ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
  in
    let
      (ContractState s'') = _STF_IP_PAM ct s' t
    in
      ContractState $ s'' # L.notionalPrincipal .~ (s ^. L.notionalPrincipal) + (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
_STF_IPCI_PAM _ s _ = s

_STF_IPCI_LAM :: forall a. EuclideanRing a => ActusOps a => ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_IPCI_LAM
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , maturityDate
        }
    )
  s'@(ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    nt' = (s ^. L.notionalPrincipal) + (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
    ipcb' = interestCalculationBase ct
      where
      interestCalculationBase (ContractTerms { interestCalculationBase: Just IPCB_NT }) = nt'
      interestCalculationBase _ = s ^. L.interestCalculationBase
  in
    let
      (ContractState s'') = _STF_IP_PAM ct s' t
    in
      ContractState $ s'' # L.notionalPrincipal .~ nt'
        # L.interestCalculationBase .~ ipcb'
_STF_IPCI_LAM _ s _ = s

---------------------------------------------
-- Interest Calculation Base Fixing (IPCB) --
---------------------------------------------

_STF_IPCB_LAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_IPCB_LAM fs ct s'@(ContractState s) t =
  let
    (ContractState s'') = _STF_PY_LAM fs ct s' t
  in
    ContractState $ s'' # L.interestCalculationBase .~ (s ^. L.notionalPrincipal)

-------------------------------
-- Rate Reset (RR) --
-------------------------------

_STF_RR_PAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_RR_PAM
  fs
  ( RiskFactors
      { o_rf_RRMO
      }
  )
  ct@
    ( ContractTerms
        { feeBasis: Just FEB_N
        , feeRate: Just fer
        , lifeFloor: Just rrlf
        , lifeCap: Just rrlc
        , periodCap: Just rrpc
        , periodFloor: Just rrpf
        , rateMultiplier: Just rrmlt
        , rateSpread: Just rrsp
        , dayCountConvention: Just dcc
        , maturityDate
        }
    )
  s'@(ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    delta_r = _min (_max (o_rf_RRMO * (_fromDecimal rrmlt) + (_fromDecimal rrsp) - (s ^. L.nominalInterest)) (_fromDecimal rrpf)) (_fromDecimal rrpc)
    ipnr' = _min (_max (s ^. L.nominalInterest + delta_r) (_fromDecimal rrlf)) (_fromDecimal rrlc)
  in
    let
      (ContractState s'') = _STF_PY_PAM fs ct s' t
    in
      ContractState $ s''
        # L.accruedInterest .~ (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
        # L.accruedFees .~ (s ^. L.accruedFees) + timeFromLastEvent * (_fromDecimal fer) * (s ^. L.notionalPrincipal)
        # L.nominalInterest .~ ipnr'
        # L.statusDate .~ t
_STF_RR_PAM
  fs'@(FeeSchedule fs)
  ( RiskFactors
      { o_rf_RRMO
      }
  )
  ct@
    ( ContractTerms
        { feeRate: Just fer
        , lifeFloor: Just rrlf
        , lifeCap: Just rrlc
        , periodCap: Just rrpc
        , periodFloor: Just rrpf
        , rateMultiplier: Just rrmlt
        , rateSpread: Just rrsp
        , dayCountConvention: Just dcc
        , contractRole
        , maturityDate
        }
    )
  s'@(ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    timeFromLatestFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment t maturityDate
    timeFromLatestToNextFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment fs.nextFeePayment maturityDate
    delta_r = _min (_max (o_rf_RRMO * (_fromDecimal rrmlt) + (_fromDecimal rrsp) - (s ^. L.nominalInterest)) (_fromDecimal rrpf)) (_fromDecimal rrpc)
    ipnr' = _min (_max (s ^. L.nominalInterest + delta_r) (_fromDecimal rrlf)) (_fromDecimal rrlc)
  in
    let
      (ContractState s'') = _STF_PY_PAM fs' ct s' t
    in
      ContractState $ s''
        # L.accruedInterest .~ (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
        # L.accruedFees .~ (if fs.latestFeePayment == fs.nextFeePayment then zero else (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * (_fromDecimal fer))
        # L.nominalInterest .~ ipnr'
        # L.statusDate .~ t
_STF_RR_PAM _ _ _ s _ = s

_STF_RR_LAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_RR_LAM
  fs
  ( RiskFactors
      { o_rf_RRMO
      }
  )
  ct@
    ( ContractTerms
        { lifeFloor: Just rrlf
        , lifeCap: Just rrlc
        , periodCap: Just rrpc
        , periodFloor: Just rrpf
        , rateMultiplier: Just rrmlt
        , rateSpread: Just rrsp
        }
    )
  s'@(ContractState s)
  t =
  let
    delta_r = _min (_max (o_rf_RRMO * (_fromDecimal rrmlt) + (_fromDecimal rrsp) - (s ^. L.nominalInterest)) (_fromDecimal rrpf)) (_fromDecimal rrpc)
    ipnr' = _min (_max ((s ^. L.nominalInterest) + delta_r) (_fromDecimal rrlf)) (_fromDecimal rrlc)
  in
    let
      (ContractState s'') = _STF_PY_LAM fs ct s' t
    in
      ContractState $ s''
        # L.nominalInterest .~ ipnr'
        # L.statusDate .~ t
_STF_RR_LAM _ _ _ s _ = s

_STF_RR_ANN :: forall a. EuclideanRing a => ActusOps a => PrincipalRedemptionSchedule -> FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_RR_ANN
  (PrincipalRedemptionSchedule prs)
  (FeeSchedule fs)
  ( RiskFactors
      { o_rf_RRMO
      }
  )
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , lifeFloor: Just rrlf
        , lifeCap: Just rrlc
        , periodCap: Just rrpc
        , periodFloor: Just rrpf
        , rateMultiplier: Just rrmlt
        , rateSpread: Just rrsp
        , feeRate
        , contractRole
        , maturityDate
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    timeFromLatestFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment t maturityDate
    timeFromLatestToNextFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment fs.nextFeePayment maturityDate
    ti = zipWith (\tn tm -> _fromDecimal $ yearFraction dcc tn tm maturityDate) prs.laterPrincipalRedemptionDates (fromMaybe Nil $ tail prs.laterPrincipalRedemptionDates)

    ipac' = (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
    feac' = feeAccrued' ct
      where
      feeAccrued' (ContractTerms { feeBasis: Just FEB_N }) = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (_fromDecimal <$> feeRate)
      feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * thisOr0 (_fromDecimal <$> feeRate)

    ipnr' = _min (_max ((s ^. L.nominalInterest) + delta_r) (_fromDecimal rrlf)) (_fromDecimal rrlc)
      where
      delta_r = _min (_max (o_rf_RRMO * (_fromDecimal rrmlt) + (_fromDecimal rrsp) - (s ^. L.nominalInterest)) (_fromDecimal rrpf)) (_fromDecimal rrpc)

    prnxt' = annuity ipnr' ti
  in
    ContractState $
      s # L.accruedInterest .~ ipac'
        # L.accruedFees .~ feac'
        # L.nominalInterest .~ ipnr'
        # L.nextPrincipalRedemptionPayment .~ prnxt'
        # L.statusDate .~ t
_STF_RR_ANN _ _ _ _ s _ = s

-----------------------------
-- Rate Reset Fixing (RRF) --
-----------------------------

_STF_RRF_PAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_RRF_PAM
  fs
  _
  ct@
    ( ContractTerms
        { nextResetRate: rrnxt
        }
    )
  s
  t =
  let
    (ContractState s') = _STF_PY_PAM fs ct s t
  in
    ContractState $ s' # L.nominalInterest .~ thisOr0 (_fromDecimal <$> rrnxt)

_STF_RRF_LAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_RRF_LAM
  fs
  _
  ct@
    ( ContractTerms
        { nextResetRate: rrnxt
        }
    )
  s
  t =
  let
    (ContractState s') = _STF_PY_LAM fs ct s t
  in
    ContractState $ s' # L.nominalInterest .~ thisOr0 (_fromDecimal <$> rrnxt)

_STF_RRF_ANN :: forall a. ActusOps a => EuclideanRing a => PrincipalRedemptionSchedule -> FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_RRF_ANN
  (PrincipalRedemptionSchedule prs)
  (FeeSchedule fs)
  _
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , nextResetRate: Just rrnxt
        , contractRole
        , maturityDate
        , feeRate
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    timeFromLatestFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment t maturityDate
    timeFromLatestToNextFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment fs.nextFeePayment maturityDate
    ti = zipWith (\tn tm -> _fromDecimal $ yearFraction dcc tn tm maturityDate) prs.laterPrincipalRedemptionDates (fromMaybe Nil $ tail prs.laterPrincipalRedemptionDates)

    ipac' = (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
    feac' = feeAccrued' ct
      where
      feeAccrued' (ContractTerms { feeBasis: Just FEB_N }) = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (_fromDecimal <$> feeRate)
      feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * thisOr0 (_fromDecimal <$> feeRate)

    ipnr' = _fromDecimal rrnxt
    prnxt' = annuity ipnr' ti
  in
    ContractState $
      s # L.accruedInterest .~ ipac'
        # L.accruedFees .~ feac'
        # L.nominalInterest .~ ipnr'
        # L.nextPrincipalRedemptionPayment .~ prnxt'
        # L.statusDate .~ t
_STF_RRF_ANN _ _ _ _ s _ = s

-------------------------------------------
-- Principal Payment Amount Fixing (PRF) --
-------------------------------------------

_STF_PRF_ANN :: forall a. EuclideanRing a => ActusOps a => PrincipalRedemptionSchedule -> FeeSchedule -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_PRF_ANN
  (PrincipalRedemptionSchedule prs)
  (FeeSchedule fs)
  ct@
    ( ContractTerms
        { dayCountConvention: Just dcc
        , contractRole
        , maturityDate
        , feeRate
        }
    )
  (ContractState s)
  t =
  let
    timeFromLastEvent = _fromDecimal $ yearFraction dcc (s ^. L.statusDate) t maturityDate
    timeFromLatestFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment t maturityDate
    timeFromLatestToNextFeePayment = _fromDecimal $ yearFraction dcc fs.latestFeePayment fs.nextFeePayment maturityDate
    timeToNextPrincipalRedemption = _fromDecimal $ yearFraction dcc t prs.nextPrincipalRedemption maturityDate
    ti = zipWith (\tn tm -> _fromDecimal $ yearFraction dcc tn tm maturityDate) prs.laterPrincipalRedemptionDates (fromMaybe Nil $ tail prs.laterPrincipalRedemptionDates)

    ipac' = (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
    feac' = feeAccrued' ct
      where
      feeAccrued' (ContractTerms { feeBasis: Just FEB_N }) = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (_fromDecimal <$> feeRate)
      feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * thisOr0 (_fromDecimal <$> feeRate)

    prnxt' = sign contractRole * frac * scale
      where
      scale = (s ^. L.notionalPrincipal) + ipac' + timeToNextPrincipalRedemption * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
      frac = annuity (s ^. L.nominalInterest) ti
  in
    ContractState $
      s # L.accruedInterest .~ ipac'
        # L.accruedFees .~ feac'
        # L.nextPrincipalRedemptionPayment .~ prnxt'
        # L.statusDate .~ t
_STF_PRF_ANN _ _ _ s _ = s

-------------------------------
-- Scaling Index Fixing (SC) --
-------------------------------

_STF_SC_PAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_SC_PAM
  fs
  ( RiskFactors
      { o_rf_SCMO
      }
  )
  ct@
    ( ContractTerms
        { scalingEffect: Just scef
        , scalingIndexAtStatusDate: Just scied
        }
    )
  s'@(ContractState s)
  t =
  let
    nsc' = case scef of
      SE_OOM -> s ^. L.notionalScalingMultiplier
      SE_IOO -> s ^. L.notionalScalingMultiplier
      _ -> (o_rf_SCMO - (_fromDecimal scied)) / (_fromDecimal scied)
    isc' = case scef of
      SE_ONO -> s ^. L.interestScalingMultiplier
      SE_OOM -> s ^. L.interestScalingMultiplier
      SE_ONM -> s ^. L.interestScalingMultiplier
      _ -> (o_rf_SCMO - (_fromDecimal scied)) / (_fromDecimal scied)
  in
    let
      (ContractState s'') = _STF_PY_PAM fs ct s' t
    in
      ContractState $ s''
        # L.notionalScalingMultiplier .~ nsc'
        # L.interestScalingMultiplier .~ isc'
_STF_SC_PAM _ _ _ s _ = s

_STF_SC_LAM :: forall a. EuclideanRing a => ActusOps a => FeeSchedule -> RiskFactors a -> ContractTerms -> ContractState a -> DateTime -> ContractState a
_STF_SC_LAM
  fs
  ( RiskFactors
      { o_rf_SCMO
      }
  )
  ct@
    ( ContractTerms
        { scalingIndexAtContractDealDate: Just sccdd
        , scalingEffect: Just scef
        }
    )
  s'@(ContractState s)
  t =
  let
    (ContractState s'') = _STF_PY_LAM fs ct s' t
  in
    ContractState $ s''
      # L.notionalScalingMultiplier .~ (if contains (Pattern "N") (show scef) then o_rf_SCMO / (_fromDecimal sccdd) else s ^. L.notionalScalingMultiplier)
      # L.interestScalingMultiplier .~ (if contains (Pattern "I") (show scef) then o_rf_SCMO / (_fromDecimal sccdd) else s ^. L.interestScalingMultiplier)
_STF_SC_LAM _ _ _ s _ = s
