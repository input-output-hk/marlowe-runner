module Actus.Utility.ScheduleGenerator
  ( generateRecurrentSchedule
  , inf
  , sup
  , (<+>)
  , (<->)
  , plus_sched
  , minus_sched
  ) where

import Prelude

import Actus.Domain (Cycle, ScheduleConfig, ShiftedSchedule, Stub(..), mkShiftedDay)
import Actus.Utility.DateShift (applyBDC, applyEOMC, shiftDate)
import Data.DateTime (DateTime)
import Data.Foldable (maximum, minimum)
import Data.List (List(..), delete, dropEnd, filter, init, last, length, snoc)
import Data.Maybe (Maybe(..), fromMaybe)

maximumMaybe :: forall a. Ord a => List a -> Maybe a
maximumMaybe Nil = Nothing
maximumMaybe xs = maximum xs

minimumMaybe :: forall a. Ord a => List a -> Maybe a
minimumMaybe Nil = Nothing
minimumMaybe xs = minimum xs

inf :: forall a. Ord a => List a -> a -> Maybe a
inf set threshold = minimumMaybe $ filter (_ > threshold) set

sup :: forall a. Ord a => List a -> a -> Maybe a
sup set threshold = maximumMaybe $ filter (_ < threshold) set

init' :: forall a. List a -> List a
init' Nil = Nil
init' ls = fromMaybe Nil $ init ls

correction :: Cycle -> DateTime -> DateTime -> List DateTime -> List DateTime
correction
  ( { stub: ShortStub
    , includeEndDay: false
    }
  )
  anchorDate
  endDate
  schedule
  | endDate == anchorDate =
      delete anchorDate $ init' schedule
correction
  ( { stub: ShortStub
    }
  )
  _
  _
  schedule = init' schedule
correction
  ( { stub: LongStub
    , includeEndDay: true
    }
  )
  _
  endDate
  schedule
  | Just endDate /= last schedule =
      let
        s = init' schedule
        l = length s
      in
        if l > 2 then dropEnd 1 s
        else s
correction
  ( { stub: LongStub
    , includeEndDay: true
    }
  )
  _
  _
  schedule = init' schedule
correction
  ( { stub: LongStub
    , includeEndDay: false
    }
  )
  anchorDate
  endDate
  schedule
  | endDate == anchorDate
      && Just endDate /= last schedule =
      let
        s = delete anchorDate $ init' schedule
        l = length s
      in
        if l > 2 then dropEnd 1 s
        else s
correction
  ( { stub: LongStub
    , includeEndDay: false
    }
  )
  anchorDate
  endDate
  schedule
  | endDate == anchorDate =
      let
        s = delete anchorDate $ init' schedule
        l = length s
      in
        if l > 2 then dropEnd 1 s
        else s
correction
  ( { stub: LongStub
    , includeEndDay: false
    }
  )
  _
  endDate
  schedule
  | Just endDate /= last schedule =
      let
        s = init' schedule
        l = length s
      in
        if l > 2 then dropEnd 1 s
        else s
correction
  ( { stub: LongStub
    , includeEndDay: false
    }
  )
  _
  _
  schedule = init' schedule

generateRecurrentSchedule' :: Cycle -> DateTime -> DateTime -> List DateTime
generateRecurrentSchedule' cycle anchorDate endDate =
  let
    go :: DateTime -> Int -> List DateTime -> List DateTime
    go current k acc =
      if current >= endDate || cycle.n == 0 then snoc acc current
      else
        let
          current' = shiftDate anchorDate (k * cycle.n) cycle.p
        in
          go current' (k + 1) (snoc acc current)
  in
    go anchorDate 1 Nil

generateRecurrentSchedule
  :: DateTime -- ^ Anchor date
  -> Cycle -- ^ Cycle
  -> DateTime -- ^ End date
  -> ScheduleConfig -- ^ Schedule config
  -> ShiftedSchedule -- ^ New schedule
generateRecurrentSchedule
  a
  c
  e
  { endOfMonthConvention: Just eomc
  , calendar: Just cal
  , businessDayConvention: Just bdc
  } =
  let
    addEndDay true endDate schedule = snoc schedule (applyBDC bdc cal endDate)
    addEndDay _ _ schedule = schedule
  in
    addEndDay c.includeEndDay e
      <<< map (applyBDC bdc cal <<< applyEOMC a c eomc)
      <<<
        correction c a e $ generateRecurrentSchedule' c a e
generateRecurrentSchedule _ _ _ _ = Nil

plus_sched :: DateTime -> Cycle -> DateTime
plus_sched d c = shiftDate d c.n c.p

infixl 8 plus_sched as <+>

minus_sched :: DateTime -> Cycle -> DateTime
minus_sched d c = shiftDate d (-c.n) c.p

infixl 8 minus_sched as <->
