-- | ACTUS contract schedules
module Actus.Model.ContractSchedule
  ( maturity
  , schedule
  ) where

import Prelude

import Actus.Domain (CT(..), ContractTerms(..), EventType(..), IPCB(..), PPEF(..), PYTP(..), SCEF(..), ShiftedDay, mkShiftedDay)
import Actus.Utility (applyBDCWithCfg, applyEOMC, generateRecurrentSchedule, inf, yearFraction, (<+>), (<->))
import Actus.Utility.DateShift (addDays')
import Control.Alt ((<|>))
import Control.Apply (lift2, lift4)
import Data.DateTime (DateTime)
import Data.Decimal (Decimal, toNumber)
import Data.List (List(..), delete, filter, find, head, nub, singleton, snoc, sortBy, (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Int (ceil)
import Data.Ord.Down (Down(..))
import Data.Tuple.Nested ((/\))

-- |Generate the schedule for a given event type
schedule
  ::
     -- | Event type
     EventType
  ->
  -- | Contract terms
  ContractTerms
  ->
  -- | Schedule
  List ShiftedDay
schedule IED ct@(ContractTerms { contractType: PAM }) = _SCHED_IED_PAM ct
schedule MD ct@(ContractTerms { contractType: PAM }) = _SCHED_MD_PAM ct
schedule PP ct@(ContractTerms { contractType: PAM }) = _SCHED_PP_PAM ct
schedule PY ct@(ContractTerms { contractType: PAM }) = _SCHED_PY_PAM ct
schedule FP ct@(ContractTerms { contractType: PAM }) = _SCHED_FP_PAM ct
schedule PRD ct@(ContractTerms { contractType: PAM }) = _SCHED_PRD_PAM ct
schedule TD ct@(ContractTerms { contractType: PAM }) = _SCHED_TD_PAM ct
schedule IP ct@(ContractTerms { contractType: PAM }) = _SCHED_IP_PAM ct
schedule IPCI ct@(ContractTerms { contractType: PAM }) = _SCHED_IPCI_PAM ct
schedule RR ct@(ContractTerms { contractType: PAM }) = _SCHED_RR_PAM ct
schedule RRF ct@(ContractTerms { contractType: PAM }) = _SCHED_RRF_PAM ct
schedule SC ct@(ContractTerms { contractType: PAM }) = _SCHED_SC_PAM ct
schedule IED ct@(ContractTerms { contractType: LAM }) = _SCHED_IED_PAM ct
schedule PR ct@(ContractTerms { contractType: LAM }) = _SCHED_PR_LAM ct
schedule MD ct@(ContractTerms { contractType: LAM }) = _SCHED_MD_LAM ct
schedule PP ct@(ContractTerms { contractType: LAM }) = _SCHED_PP_PAM ct
schedule PY ct@(ContractTerms { contractType: LAM }) = _SCHED_PY_PAM ct
schedule FP ct@(ContractTerms { contractType: LAM }) = _SCHED_FP_PAM ct
schedule PRD ct@(ContractTerms { contractType: LAM }) = _SCHED_PRD_PAM ct
schedule TD ct@(ContractTerms { contractType: LAM }) = _SCHED_TD_PAM ct
schedule IP ct@(ContractTerms { contractType: LAM }) = _SCHED_IP_PAM ct
schedule IPCI ct@(ContractTerms { contractType: LAM }) = _SCHED_IPCI_PAM ct
schedule IPCB ct@(ContractTerms { contractType: LAM }) = _SCHED_IPCB_LAM ct
schedule RR ct@(ContractTerms { contractType: LAM }) = _SCHED_RR_PAM ct
schedule RRF ct@(ContractTerms { contractType: LAM }) = _SCHED_RRF_PAM ct
schedule SC ct@(ContractTerms { contractType: LAM }) = _SCHED_SC_PAM ct
schedule IED ct@(ContractTerms { contractType: NAM }) = _SCHED_IED_PAM ct
schedule PR ct@(ContractTerms { contractType: NAM }) = _SCHED_PR_LAM ct
schedule MD ct@(ContractTerms { contractType: NAM }) = _SCHED_MD_PAM ct
schedule PP ct@(ContractTerms { contractType: NAM }) = _SCHED_PP_PAM ct
schedule PY ct@(ContractTerms { contractType: NAM }) = _SCHED_PY_PAM ct
schedule FP ct@(ContractTerms { contractType: NAM }) = _SCHED_FP_PAM ct
schedule PRD ct@(ContractTerms { contractType: NAM }) = _SCHED_PRD_PAM ct
schedule TD ct@(ContractTerms { contractType: NAM }) = _SCHED_TD_PAM ct
schedule IP ct@(ContractTerms { contractType: NAM }) = _SCHED_IP_NAM ct
schedule IPCI ct@(ContractTerms { contractType: NAM }) = _SCHED_IPCI_NAM ct
schedule IPCB ct@(ContractTerms { contractType: NAM }) = _SCHED_IPCB_LAM ct
schedule RR ct@(ContractTerms { contractType: NAM }) = _SCHED_RR_PAM ct
schedule RRF ct@(ContractTerms { contractType: NAM }) = _SCHED_RRF_PAM ct
schedule SC ct@(ContractTerms { contractType: NAM }) = _SCHED_SC_PAM ct
schedule IED ct@(ContractTerms { contractType: ANN }) = _SCHED_IED_PAM ct
schedule PR ct@(ContractTerms { contractType: ANN }) = _SCHED_PR_LAM ct
schedule MD ct@(ContractTerms { contractType: ANN }) = _SCHED_MD_PAM ct
schedule PP ct@(ContractTerms { contractType: ANN }) = _SCHED_PP_PAM ct
schedule PY ct@(ContractTerms { contractType: ANN }) = _SCHED_PY_PAM ct
schedule FP ct@(ContractTerms { contractType: ANN }) = _SCHED_FP_PAM ct
schedule PRD ct@(ContractTerms { contractType: ANN }) = _SCHED_PRD_PAM ct
schedule TD ct@(ContractTerms { contractType: ANN }) = _SCHED_TD_PAM ct
schedule IP ct@(ContractTerms { contractType: ANN }) = _SCHED_IP_NAM ct
schedule IPCI ct@(ContractTerms { contractType: ANN }) = _SCHED_IPCI_PAM ct
schedule IPCB ct@(ContractTerms { contractType: ANN }) = _SCHED_IPCB_LAM ct
schedule RR ct@(ContractTerms { contractType: ANN }) = _SCHED_RR_PAM ct
schedule RRF ct@(ContractTerms { contractType: ANN }) = _SCHED_RRF_PAM ct
schedule SC ct@(ContractTerms { contractType: ANN }) = _SCHED_SC_PAM ct
schedule PRF ct@(ContractTerms { contractType: ANN }) = _SCHED_PRF_ANN ct
schedule _ _ = Nil

-- |Determine the maturity of a contract
maturity
  ::
     -- | Contract terms
     ContractTerms
  ->
  -- | Maturity, if available
  Maybe DateTime
maturity (ContractTerms { contractType: PAM, maturityDate }) = maturityDate
maturity (ContractTerms { contractType: LAM, maturityDate: md@(Just _) }) = md
maturity
  ( ContractTerms
      { contractType: LAM
      , maturityDate: Nothing
      , cycleAnchorDateOfPrincipalRedemption: Just pranx
      , cycleOfInterestPayment: Just ipcl
      , cycleOfPrincipalRedemption: Just prcl
      , nextPrincipalRedemptionPayment: Just prnxt
      , notionalPrincipal: Just nt
      , statusDate
      , scheduleConfig: sc@{ endOfMonthConvention }
      }
  ) =
  let
    (lastEvent /\ remainingPeriods) =
      if pranx < statusDate then
        let
          previousEvents = generateRecurrentSchedule pranx prcl statusDate sc
          f1 = (\{ calculationDay } -> calculationDay > statusDate <-> ipcl)
          f2 = (\{ calculationDay } -> calculationDay == statusDate)
          lastEventCalcDay = map _.calculationDay <<< head <<< filter f2 <<< filter f1 $ previousEvents
        in
          (lastEventCalcDay /\ nt / prnxt)
      else (Just pranx /\ (nt / prnxt - one))
  in
    do
      endOfMonthConvention' <- endOfMonthConvention
      lastEvent' <- lastEvent
      pure $ applyEOMC lastEvent' prcl endOfMonthConvention' $ lastEvent' <+> (prcl { n = prcl.n * ceiling remainingPeriods })
maturity (ContractTerms { contractType: NAM, maturityDate: md@(Just _) }) = md
maturity
  ( ContractTerms
      { contractType: NAM
      , maturityDate: Nothing
      , cycleAnchorDateOfPrincipalRedemption: Just pranx
      , nextPrincipalRedemptionPayment: Just prnxt
      , initialExchangeDate: Just ied
      , cycleOfPrincipalRedemption: Just prcl
      , notionalPrincipal: Just nt
      , nominalInterestRate: Just ipnr
      , dayCountConvention: Just dcc
      , statusDate
      , scheduleConfig: sc@{ endOfMonthConvention }
      }
  ) =
  let
    lastEvent
      | pranx >= statusDate = Just pranx
      | ied <+> prcl >= statusDate = Just $ ied <+> prcl
      | otherwise =
          let
            previousEvents = generateRecurrentSchedule pranx prcl statusDate sc
            f = (\{ calculationDay } -> calculationDay == statusDate)
          in
            map _.calculationDay <<< head <<< filter f $ previousEvents
  in
    do
      endOfMonthConvention' <- endOfMonthConvention
      lastEvent' <- lastEvent
      let yLastEventPlusPRCL = yearFraction dcc lastEvent' (lastEvent' <+> prcl) Nothing
      let redemptionPerCycle = prnxt - (yLastEventPlusPRCL * ipnr * nt)
      let remainingPeriods = ceiling $ (nt / redemptionPerCycle) - one
      pure $ applyEOMC lastEvent' prcl endOfMonthConvention' (lastEvent' <+> prcl { n = prcl.n * remainingPeriods })
maturity
  ( ContractTerms
      { contractType: ANN
      , amortizationDate: Nothing
      , maturityDate: Nothing
      , cycleAnchorDateOfPrincipalRedemption: Just pranx
      , nextPrincipalRedemptionPayment: Just prnxt
      , initialExchangeDate: Just ied
      , cycleOfPrincipalRedemption: Just prcl
      , notionalPrincipal: Just nt
      , nominalInterestRate: Just ipnr
      , dayCountConvention: Just dcc
      , statusDate
      , scheduleConfig
      }
  ) =
  let
    tplus = ied <+> prcl
    lastEvent
      | pranx >= statusDate = pranx
      | tplus >= statusDate = tplus
      | otherwise =
          let
            previousEvents = generateRecurrentSchedule statusDate prcl pranx scheduleConfig
          in
            fromMaybe ied $ head <<< sortBy (comparing Down) <<< map _.calculationDay <<< filter (\{ calculationDay } -> calculationDay > statusDate) $ previousEvents
    timeFromLastEventPlusOneCycle = yearFraction dcc lastEvent (lastEvent <+> prcl) Nothing
    redemptionPerCycle = prnxt - timeFromLastEventPlusOneCycle * ipnr * nt
    remainingPeriods = ceiling $ (nt / redemptionPerCycle) - one
  in
    Just <<< _.calculationDay <<< applyBDCWithCfg scheduleConfig $ lastEvent <+> prcl { n = remainingPeriods }
maturity
  ( ContractTerms
      { contractType: ANN
      , amortizationDate: ad@(Just _)
      }
  ) = ad
maturity
  ( ContractTerms
      { contractType: ANN
      , amortizationDate: Nothing
      , maturityDate: md@(Just _)
      }
  ) = md
maturity _ = Nothing

-- Principal at Maturity (PAM)

_SCHED_IED_PAM :: ContractTerms -> List ShiftedDay
_SCHED_IED_PAM
  ( ContractTerms
      { scheduleConfig
      , initialExchangeDate: Just ied
      }
  ) = singleton $ applyBDCWithCfg scheduleConfig ied
_SCHED_IED_PAM _ = Nil

_SCHED_MD_PAM :: ContractTerms -> List ShiftedDay
_SCHED_MD_PAM
  ct@
    ( ContractTerms
        { maturityDate
        , scheduleConfig
        }
    ) = case maturityDate <|> maturity ct of
  Just m -> singleton $ applyBDCWithCfg scheduleConfig m
  Nothing -> Nil

_SCHED_PP_PAM :: ContractTerms -> List ShiftedDay
_SCHED_PP_PAM
  ( ContractTerms
      { prepaymentEffect: Just PPEF_N
      }
  ) = Nil
_SCHED_PP_PAM
  ( ContractTerms
      { cycleAnchorDateOfOptionality: Just opanx
      , cycleOfOptionality: Just opcl
      , maturityDate: Just md
      , scheduleConfig
      }
  ) = generateRecurrentSchedule opanx opcl md scheduleConfig
_SCHED_PP_PAM
  ( ContractTerms
      { cycleAnchorDateOfOptionality: Nothing
      , cycleOfOptionality: Just opcl
      , maturityDate: Just md
      , initialExchangeDate: Just ied
      , scheduleConfig
      }
  ) = generateRecurrentSchedule (ied <+> opcl) opcl md scheduleConfig
_SCHED_PP_PAM _ = Nil

_SCHED_PY_PAM :: ContractTerms -> List ShiftedDay
_SCHED_PY_PAM
  ( ContractTerms
      { penaltyType: Just PYTP_O
      }
  ) = Nil
_SCHED_PY_PAM ct = _SCHED_PP_PAM ct

_SCHED_FP_PAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_FP_PAM
  ( ContractTerms
      { feeRate: Nothing
      }
  ) = Nil
_SCHED_FP_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfFee: Just feanx
        , cycleOfFee: Just fecl
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule feanx fecl { includeEndDay = true } m scheduleConfig
  Nothing -> Nil
_SCHED_FP_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfFee: Nothing
        , cycleOfFee: Just fecl
        , initialExchangeDate: Just ied
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule (ied <+> fecl) fecl { includeEndDay = true } m scheduleConfig
  Nothing -> Nil
_SCHED_FP_PAM _ = Nil

_SCHED_PRD_PAM :: ContractTerms -> List ShiftedDay
_SCHED_PRD_PAM
  ( ContractTerms
      { scheduleConfig
      , purchaseDate: Just prd
      }
  ) = singleton $ applyBDCWithCfg scheduleConfig prd
_SCHED_PRD_PAM _ = Nil

_SCHED_TD_PAM :: ContractTerms -> List ShiftedDay
_SCHED_TD_PAM
  ( ContractTerms
      { scheduleConfig
      , terminationDate: Just td
      }
  ) = singleton $ applyBDCWithCfg scheduleConfig td
_SCHED_TD_PAM _ = Nil

_SCHED_IP_PAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_IP_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfInterestPayment: Just ipanx
        , cycleOfInterestPayment: Just ipcl
        , capitalizationEndDate: ipced
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      s = generateRecurrentSchedule ipanx ipcl { includeEndDay = true } m scheduleConfig
    in
      filter (\{ calculationDay } -> Just calculationDay > ipced) s
  Nothing -> Nil
_SCHED_IP_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfInterestPayment: Nothing
        , cycleOfInterestPayment: Just ipcl
        , initialExchangeDate: Just ied
        , capitalizationEndDate: ipced
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      s = generateRecurrentSchedule (ied <+> ipcl) ipcl { includeEndDay = true } m scheduleConfig
    in
      filter (\{ calculationDay } -> Just calculationDay > ipced) s
  Nothing -> Nil
_SCHED_IP_PAM _ = Nil

_SCHED_IPCI_PAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_IPCI_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfInterestPayment: Just ipanx
        , cycleOfInterestPayment: Just ipcl
        , capitalizationEndDate: Just ipced
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      s = generateRecurrentSchedule ipanx ipcl { includeEndDay = true } m scheduleConfig
    in
      filter (\{ calculationDay } -> calculationDay < ipced) s `snoc` applyBDCWithCfg scheduleConfig ipced
  Nothing -> Nil
_SCHED_IPCI_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfInterestPayment: Nothing
        , cycleOfInterestPayment: Just ipcl
        , initialExchangeDate: Just ied
        , capitalizationEndDate: Just ipced
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      s = generateRecurrentSchedule (ied <+> ipcl) ipcl { includeEndDay = true } m scheduleConfig
    in
      filter (\{ calculationDay } -> calculationDay < ipced) s `snoc` applyBDCWithCfg scheduleConfig ipced
  Nothing -> Nil
_SCHED_IPCI_PAM _ = Nil

_SCHED_RR_PAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_RR_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfRateReset: Just rranx
        , cycleOfRateReset: Just rrcl
        , nextResetRate: Just _
        , statusDate
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      tt = generateRecurrentSchedule rranx rrcl { includeEndDay = false } m scheduleConfig
    in
      fromMaybe Nil (inf tt (mkShiftedDay statusDate) <#> flip delete tt)
  Nothing -> Nil
_SCHED_RR_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfRateReset: Just rranx
        , cycleOfRateReset: Just rrcl
        , nextResetRate: Nothing
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule rranx rrcl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_RR_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfRateReset: Nothing
        , cycleOfRateReset: Just rrcl
        , nextResetRate: Just _
        , initialExchangeDate: Just ied
        , statusDate
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      tt = generateRecurrentSchedule (ied <+> rrcl) rrcl { includeEndDay = false } m scheduleConfig
    in
      fromMaybe Nil (inf tt (mkShiftedDay statusDate) <#> flip delete tt)
  Nothing -> Nil
_SCHED_RR_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfRateReset: Nothing
        , cycleOfRateReset: Just rrcl
        , nextResetRate: Nothing
        , initialExchangeDate: Just ied
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule (ied <+> rrcl) rrcl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_RR_PAM
  ( ContractTerms
      { cycleAnchorDateOfRateReset: Just rranx
      , cycleOfRateReset: Nothing
      , scheduleConfig
      }
  ) = singleton $ applyBDCWithCfg scheduleConfig rranx -- if no cycle then only start (if specified) and end dates (see ScheduleFactory.java)
_SCHED_RR_PAM _ = Nil

_SCHED_RRF_PAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_RRF_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfRateReset: Just rranx
        , cycleOfRateReset: Just rrcl
        , nextResetRate: Just _
        , statusDate
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      tt = generateRecurrentSchedule rranx rrcl { includeEndDay = false } m scheduleConfig
    in
      maybeToList (find (\{ calculationDay } -> calculationDay > statusDate) tt)
  Nothing -> Nil
_SCHED_RRF_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfRateReset: Nothing
        , cycleOfRateReset: Just rrcl
        , nextResetRate: Just _
        , initialExchangeDate: Just ied
        , statusDate
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m ->
    let
      tt = generateRecurrentSchedule (ied <+> rrcl) rrcl m scheduleConfig
    in
      maybeToList (find (\{ calculationDay } -> calculationDay > statusDate) tt)
  Nothing -> Nil
_SCHED_RRF_PAM _ = Nil

_SCHED_SC_PAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_SC_PAM (ContractTerms { scalingEffect: Just SE_OOO }) = Nil
_SCHED_SC_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfScalingIndex: Just scanx
        , cycleOfScalingIndex: Just sccl
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule scanx sccl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_SC_PAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfScalingIndex: Nothing
        , cycleOfScalingIndex: Just sccl
        , initialExchangeDate: Just ied
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule (ied <+> sccl) sccl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_SC_PAM _ = Nil

-- Linear Amortizer (LAM)

_SCHED_PR_LAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_PR_LAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfPrincipalRedemption: Just pranx
        , cycleOfPrincipalRedemption: Just prcl
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule pranx prcl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_PR_LAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfPrincipalRedemption: Nothing
        , cycleOfPrincipalRedemption: Just prcl
        , maturityDate
        , initialExchangeDate: Just ied
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule (ied <+> prcl) prcl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_PR_LAM _ = Nil

_SCHED_MD_LAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_MD_LAM
  ct@
    ( ContractTerms
        { maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> singleton $ applyBDCWithCfg scheduleConfig m
  Nothing -> Nil

_SCHED_IPCB_LAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_IPCB_LAM (ContractTerms ct) | ct.interestCalculationBase /= Just IPCB_NTL = Nil
_SCHED_IPCB_LAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfInterestCalculationBase: Just ipcbanx
        , cycleOfInterestCalculationBase: Just ipcbcl
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule ipcbanx ipcbcl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_IPCB_LAM
  ct@
    ( ContractTerms
        { cycleAnchorDateOfInterestCalculationBase: Nothing
        , cycleOfInterestCalculationBase: Just ipcbcl
        , initialExchangeDate: Just ied
        , maturityDate
        , scheduleConfig
        }
    ) = case maturity ct <|> maturityDate of
  Just m -> generateRecurrentSchedule (ied <+> ipcbcl) ipcbcl { includeEndDay = false } m scheduleConfig
  Nothing -> Nil
_SCHED_IPCB_LAM _ = Nil

-- Negative Amortizer (NAM)

_SCHED_IP_NAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_IP_NAM ct@(ContractTerms { maturityDate, initialExchangeDate, cycleOfPrincipalRedemption, cycleAnchorDateOfPrincipalRedemption, scheduleConfig, cycleAnchorDateOfInterestPayment, cycleOfInterestPayment, capitalizationEndDate }) =
  let
    m = maturityDate <|> maturity ct
    s
      | isNothing cycleAnchorDateOfPrincipalRedemption = lift2 (<+>) initialExchangeDate cycleOfPrincipalRedemption
      | otherwise = cycleAnchorDateOfPrincipalRedemption

    v = lift4 generateRecurrentSchedule s cycleOfPrincipalRedemption m (Just scheduleConfig)

    r
      | isJust cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment
      | isJust cycleOfInterestPayment = lift2 (<+>) initialExchangeDate cycleOfInterestPayment
      | otherwise = Nothing

    _T = lift2 (<->) s cycleOfPrincipalRedemption

    u
      | isNothing cycleAnchorDateOfInterestPayment && isNothing cycleOfInterestPayment = Nothing
      | isJust capitalizationEndDate && Just true == lift2 (>) capitalizationEndDate _T = Nothing
      | otherwise = lift4 generateRecurrentSchedule r ((\c -> c { includeEndDay = true }) <$> cycleOfInterestPayment) m (Just scheduleConfig)

    result = nub <$> lift2 (++) u v

    result'
      | isJust result && isJust capitalizationEndDate = filter (\{ calculationDay } -> Just calculationDay > capitalizationEndDate) <$> result
      | otherwise = result
  in
    fromMaybe Nil result'

_SCHED_IPCI_NAM
  :: ContractTerms
  -> List ShiftedDay
_SCHED_IPCI_NAM ct@(ContractTerms { maturityDate, initialExchangeDate, cycleOfPrincipalRedemption, cycleAnchorDateOfPrincipalRedemption, scheduleConfig, capitalizationEndDate, cycleOfInterestPayment, cycleAnchorDateOfInterestPayment }) =
  let
    m = maturity ct <|> maturityDate
    s
      | isNothing cycleAnchorDateOfPrincipalRedemption = lift2 (<+>) initialExchangeDate cycleOfPrincipalRedemption
      | otherwise = cycleAnchorDateOfPrincipalRedemption

    v = lift4 generateRecurrentSchedule s cycleOfPrincipalRedemption m (Just scheduleConfig)

    r
      | isJust capitalizationEndDate = capitalizationEndDate
      | isJust cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment
      | isJust cycleOfInterestPayment = lift2 (<+>) initialExchangeDate cycleOfInterestPayment
      | otherwise = Nothing

    _T = lift2 (<->) s cycleOfPrincipalRedemption

    u
      | isNothing cycleAnchorDateOfInterestPayment && isNothing cycleOfInterestPayment = Nothing
      | isJust capitalizationEndDate && Just true == lift2 (>) capitalizationEndDate _T = Nothing
      | otherwise = lift4 generateRecurrentSchedule r ((\c -> c { includeEndDay = true }) <$> cycleOfInterestPayment) m (Just scheduleConfig)

    result = Just $ nub (fromMaybe Nil u ++ fromMaybe Nil v)

    result'
      | isJust result && isJust capitalizationEndDate = filter (\{ calculationDay } -> Just calculationDay <= capitalizationEndDate) <$> result
      | otherwise = Nothing
  in
    fromMaybe Nil result'

-- Annuity (ANN)

_SCHED_PRF_ANN
  :: ContractTerms
  -> List ShiftedDay
_SCHED_PRF_ANN
  ct@
    ( ContractTerms
        { cycleAnchorDateOfPrincipalRedemption: Just pranx
        , nextPrincipalRedemptionPayment: Nothing
        , initialExchangeDate: Just ied
        }
    ) =
  let
    prf
      | pranx > ied = let p = addDays' (-1) pranx in singleton $ { calculationDay: p, paymentDay: p }
      | otherwise = Nil
    rr = _SCHED_RR_PAM ct
    rrf = _SCHED_RRF_PAM ct
  in
    prf ++ rr ++ rrf
_SCHED_PRF_ANN _ = Nil

append' :: forall a. List a -> List a -> List a
append' Nil ys = ys
append' (x : xs) ys = x : (append' xs ys)

infixl 8 append' as ++

maybeToList :: forall a. Maybe a -> List a
maybeToList (Just x) = singleton x
maybeToList Nothing = mempty

ceiling :: Decimal -> Int
ceiling = ceil <<< toNumber
