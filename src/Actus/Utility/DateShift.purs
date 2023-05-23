module Actus.Utility.DateShift
  ( addDays
  , addDays'
  , applyBDC
  , applyBDCWithCfg
  , applyEOMC
  , getFollowingBusinessDay
  , getPreceedingBusinessDay
  , moveToEndOfMonth
  , shiftDate
  ) where

import Prelude

import Actus.Domain (BDC(..), Calendar(..), Cycle, EOMC(..), Period(..), ScheduleConfig, ShiftedDay)
import Data.Date (Date, Weekday(..), Year, canonicalDate, day, lastDayOfMonth, month, weekday, year)
import Data.DateTime (DateTime(..))
import Data.Enum (class BoundedEnum, fromEnum, succ, pred, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

{- Business Day Convention -}

applyBDCWithCfg :: ScheduleConfig -> DateTime -> ShiftedDay
applyBDCWithCfg
  { businessDayConvention: Just bdc
  , calendar: Just cal
  }
  d = applyBDC bdc cal d
applyBDCWithCfg _ date = { paymentDay: date, calculationDay: date }

applyBDC :: BDC -> Calendar -> DateTime -> ShiftedDay
applyBDC BDC_NULL _ date = { paymentDay: date, calculationDay: date }

applyBDC BDC_SCF cal date =
  { paymentDay: getFollowingBusinessDay date cal
  , calculationDay: getFollowingBusinessDay date cal
  }

applyBDC BDC_SCMF cal date =
  { paymentDay: shiftModifiedFollowing date cal
  , calculationDay: shiftModifiedFollowing date cal
  }

applyBDC BDC_CSF cal date =
  { paymentDay: getFollowingBusinessDay date cal
  , calculationDay: date
  }

applyBDC BDC_CSMF cal date =
  { paymentDay: shiftModifiedFollowing date cal
  , calculationDay: date
  }

applyBDC BDC_SCP cal date =
  { paymentDay: getPreceedingBusinessDay date cal
  , calculationDay: getPreceedingBusinessDay date cal
  }

applyBDC BDC_SCMP cal date =
  { paymentDay: shiftModifiedPreceeding date cal
  , calculationDay: shiftModifiedPreceeding date cal
  }

applyBDC BDC_CSP cal date =
  { paymentDay: getPreceedingBusinessDay date cal
  , calculationDay: date
  }

applyBDC BDC_CSMP cal date =
  { paymentDay: shiftModifiedPreceeding date cal
  , calculationDay: date
  }

shiftModifiedFollowing :: DateTime -> Calendar -> DateTime
shiftModifiedFollowing dt@(DateTime d _) cal =
  let
    m = month d
    st@(DateTime d' _) = getFollowingBusinessDay dt cal
    shiftedMonth = month d'
  in
    if m == shiftedMonth then st
    else getPreceedingBusinessDay dt cal

shiftModifiedPreceeding :: DateTime -> Calendar -> DateTime
shiftModifiedPreceeding dt@(DateTime d _) cal =
  let
    m = month d
    st@(DateTime d' _) = getPreceedingBusinessDay dt cal
    shiftedMonth = month d'
  in
    if m == shiftedMonth then st else getFollowingBusinessDay dt cal

getFollowingBusinessDay :: DateTime -> Calendar -> DateTime
getFollowingBusinessDay (DateTime d t) CLDR_MF =
  let
    d' = case weekday d of
      Saturday -> addDays 2 d
      Sunday -> addDays 1 d
      _ -> d
  in
    DateTime d' t
getFollowingBusinessDay dt _ = dt

getPreceedingBusinessDay :: DateTime -> Calendar -> DateTime
getPreceedingBusinessDay (DateTime d t) CLDR_MF =
  let
    d' = case weekday d of
      Saturday -> addDays (-1) d
      Sunday -> addDays (-2) d
      _ -> d
  in
    DateTime d' t
getPreceedingBusinessDay dt _ = dt

shiftDate :: DateTime -> Int -> Period -> DateTime
shiftDate dt n p =
  case p of
    P_D -> addDays' n dt
    P_W -> addDays' (n * 7) dt
    P_M -> addGregorianMonthsClip n dt
    P_Q -> addGregorianMonthsClip (n * 3) dt
    P_H -> addGregorianMonthsClip (n * 6) dt
    P_Y -> addGregorianYearsClip n dt

{- End of Month Convention -}
applyEOMC :: DateTime -> Cycle -> EOMC -> DateTime -> DateTime
applyEOMC s cycle endOfMonthConvention dt
  | isLastDayOfMonthWithLessThan31Days s
      && cycle.p /= P_D
      && cycle.p /= P_W
      && endOfMonthConvention == EOMC_EOM = moveToEndOfMonth dt
  | otherwise = dt

isLastDayOfMonthWithLessThan31Days :: DateTime -> Boolean
isLastDayOfMonthWithLessThan31Days (DateTime d _) = Just (day d) < toEnum 31 && isLastDayOfMonth d

moveToEndOfMonth :: DateTime -> DateTime
moveToEndOfMonth (DateTime d t) = DateTime (canonicalDate (year d) (month d) (lastDayOfMonth (year d) (month d))) t

isLastDayOfMonth :: Date -> Boolean
isLastDayOfMonth d = lastDayOfMonth (year d) (month d) == (day d)

addDays :: Int -> Date -> Date
addDays n d | n > 0 = addDays (n - 1) (unsafePartial fromJust $ succ d)
addDays n d | n < 0 = addDays (n + 1) (unsafePartial fromJust $ pred d)
addDays _ d = d

addDays' :: Int -> DateTime -> DateTime
addDays' n (DateTime d t) = DateTime (addDays n d) t

unsafeToEnum :: forall d. BoundedEnum d => Int -> d
unsafeToEnum = unsafePartial fromJust <<< toEnum

rolloverMonths :: Year /\ Int -> Int /\ Int
rolloverMonths (y /\ m) = (fromEnum y + (div (m - 1) 12)) /\ ((mod (m - 1) 12) + 1)

addGregorianMonths :: Int -> Date -> Int /\ Int /\ Int
addGregorianMonths n d = y' /\ m' /\ fromEnum (day d)
  where
  (y' /\ m') = rolloverMonths (year d /\ (fromEnum (month d) + n))

addGregorianMonthsClip :: Int -> DateTime -> DateTime
addGregorianMonthsClip n (DateTime d t) =
  let
    y' = unsafeToEnum y
    m' = unsafeToEnum m
    d' = unsafeToEnum a
    ld = lastDayOfMonth y' m'
    da =
      if ld < d' then canonicalDate y' m' ld
      else canonicalDate y' m' d'
  in
    DateTime da t
  where
  (y /\ m /\ a) = addGregorianMonths n d

addGregorianYearsClip :: Int -> DateTime -> DateTime
addGregorianYearsClip n = addGregorianMonthsClip (n * 12)
