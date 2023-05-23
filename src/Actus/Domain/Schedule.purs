module Actus.Domain.Schedule where

import Data.DateTime (DateTime)
import Data.List (List)

type ShiftedDay =
  { paymentDay :: DateTime
  , calculationDay :: DateTime
  }

mkShiftedDay :: DateTime -> ShiftedDay
mkShiftedDay d = { paymentDay: d, calculationDay: d }

type ShiftedSchedule = List ShiftedDay
