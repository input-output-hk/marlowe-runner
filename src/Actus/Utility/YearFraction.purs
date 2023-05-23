module Actus.Utility.YearFraction
  ( yearFraction
  ) where

import Prelude

import Actus.Domain.ContractTerms (DCC(..))
import Data.Date (Date, Month(..), diff, isLeapYear, lastDayOfMonth, year)
import Data.DateTime (DateTime(..), canonicalDate, date, day, month)
import Data.DateTime as DateTime
import Data.Decimal (Decimal)
import Data.Enum (fromEnum, toEnum)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Refined (fromInt)
import Data.Time (Time(..))
import Data.Time.Duration (Days(..))
import Data.Time.Duration (Seconds(..)) as Duration
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

yearFraction :: DCC -> DateTime -> DateTime -> Maybe DateTime -> Decimal
yearFraction dcc x y o = yearFraction' dcc (date x) (date $ clipToMidnight y) (date <$> o)

yearFraction' :: DCC -> Date -> Date -> Maybe Date -> Decimal
yearFraction' DCC_A_AISDA startDay endDay _
  | startDay <= endDay =
      let
        d1Year = year startDay
        d2Year = year endDay

        d1YearFraction = fromInt $ if isLeapYear d1Year then 366 else 365
        d2YearFraction = fromInt $ if isLeapYear d2Year then 366 else 365
      in
        if d1Year == d2Year then
          let
            Days d = diff endDay startDay
          in
            (fromInt $ ceil d) / d1YearFraction
        else
          let
            (d1YearLastDay :: Date) = canonicalDate (fromMaybe (year startDay) $ toEnum $ (fromEnum d1Year) + 1) January (fromMaybe (day startDay) $ toEnum 1)
            (d2YearLastDay :: Date) = canonicalDate d2Year January (fromMaybe (day endDay) $ toEnum 1)

            (firstFractionDays :: Decimal) = let Days s = diff d1YearLastDay startDay in fromInt $ ceil s
            (secondFractionDays :: Decimal) = let Days s = diff endDay d2YearLastDay in fromInt $ ceil s
          in
            (firstFractionDays / d1YearFraction)
              + (secondFractionDays / d2YearFraction)
              + (fromInt $ fromEnum d2Year)
              - (fromInt $ fromEnum d1Year)
              - one
  | otherwise = zero

yearFraction' DCC_A_360 startDay endDay _
  | startDay <= endDay = let Days daysDiff = diff endDay startDay in (fromInt $ ceil daysDiff) / fromInt 360
  | otherwise = zero

yearFraction' DCC_A_365 startDay endDay _
  | startDay <= endDay = let Days daysDiff = diff endDay startDay in (fromInt $ ceil daysDiff) / fromInt 365
  | otherwise = zero

yearFraction' DCC_E30_360ISDA _ _ Nothing = error "DCC_E30_360ISDA requires maturity date"
yearFraction' DCC_E30_360ISDA startDay endDay (Just maturityDate)
  | startDay <= endDay =
      let
        d1ChangedDay = if isLastDayOfMonth startDay then 30 else fromEnum $ day startDay
        d2ChangedDay = if isLastDayOfMonth endDay && not (endDay == maturityDate && (Just (month endDay) == toEnum 2)) then 30 else fromEnum $ day endDay
      in
        ( fromInt $
            360
              * ((fromEnum $ year endDay) - (fromEnum $ year startDay))
              + 30
                  * ((fromEnum $ month endDay) - (fromEnum $ month startDay))
              + (d2ChangedDay - d1ChangedDay)
        )
          / (fromInt 360)
  | otherwise = zero

yearFraction' DCC_E30_360 startDay endDay _
  | startDay <= endDay =
      let
        d1ChangedDay = if fromEnum (day startDay) == 31 then 30 else fromEnum (day startDay)
        d2ChangedDay = if fromEnum (day endDay) == 31 then 30 else fromEnum (day endDay)
      in
        ( fromInt $
            360
              * ((fromEnum $ year endDay) - (fromEnum $ year startDay))
              + 30
                  * ((fromEnum $ month endDay) - (fromEnum $ month startDay))
              + (d2ChangedDay - d1ChangedDay)
        ) / (fromInt 360)
  | otherwise = zero

yearFraction' dcc _ _ _ = error $ "Unsupported day count convention: " <> show dcc

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

isLastDayOfMonth :: Date -> Boolean
isLastDayOfMonth d = lastDayOfMonth (year d) (month d) == (day d)

-- |Advance to midnight, if one second before midnight - see note in ACTUS specification (2.8. Date/Time)
clipToMidnight :: DateTime -> DateTime
clipToMidnight dt@(DateTime _ time) = fromMaybe dt $ do
  nearlyMidnight <- Time
    <$> toEnum 23
    <*> toEnum 59
    <*> toEnum 59
    <*> toEnum 0
  if time == nearlyMidnight then DateTime.adjust (Duration.Seconds 1.0) dt
  else pure dt
