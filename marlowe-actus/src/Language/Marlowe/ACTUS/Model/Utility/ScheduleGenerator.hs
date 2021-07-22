{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator
  ( generateRecurrentScheduleWithCorrections
  , plusCycle
  , minusCycle
  , sup
  , inf
  , remove
  )
where

import           Control.Arrow                                    ((>>>))
import           Data.Function                                    ((&))
import qualified Data.List                                        as L (delete, init, last, length)
import           Data.Maybe                                       (fromJust)
import           Data.Time.Calendar                               (Day, addDays, addGregorianMonthsClip,
                                                                   addGregorianYearsClip, fromGregorian,
                                                                   gregorianMonthLength, toGregorian)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms (Cycle (..), EOMC (EOMC_EOM), Period (..),
                                                                   ScheduleConfig (..), Stub (LongStub))
import           Language.Marlowe.ACTUS.Definitions.Schedule      (ShiftedDay (..), ShiftedSchedule)
import           Language.Marlowe.ACTUS.Model.Utility.DateShift   (applyBDC)


maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe xs = Just $ minimum xs

inf :: [ShiftedDay] -> Day -> Maybe ShiftedDay
inf set threshold =
  minimumMaybe [t | t <- set, calculationDay t >= threshold]

sup :: [ShiftedDay] -> Day -> Maybe ShiftedDay
sup set threshold =
  maximumMaybe [t | t <- set, calculationDay t <= threshold]

remove :: ShiftedDay -> [ShiftedDay] -> [ShiftedDay]
remove d = filter (\t -> calculationDay t /= calculationDay d)

correction :: Cycle -> Day -> Day -> [Day] -> [Day]
correction Cycle{ stub = stub, includeEndDay = includeEndDay} anchorDate endDate schedule =
  let
    lastDate = L.last schedule
    schedule' = L.init schedule
    schedule'Size = L.length schedule'
    schedule'' =
      if includeEndDay then
        schedule' ++ [endDate]
      else
        if endDate == anchorDate then
          L.delete anchorDate schedule'
        else
          schedule'
  in
    if stub == LongStub && L.length schedule'' > 2 && endDate /= lastDate then
      L.delete (schedule'' !! (schedule'Size - 1)) schedule''
    else
      schedule''

generateRecurrentSchedule :: Cycle -> Day -> Day -> [Day]
generateRecurrentSchedule Cycle {..} anchorDate endDate =
  let go :: Day -> Integer -> [Day] -> [Day]
      go current k acc = if current >= endDate
        then acc ++ [current]
        else
          (let current' = shiftDate anchorDate (k * n) p
           in  go current' (k + 1) (acc ++ [current])
          )
  in  go anchorDate 1 []

generateRecurrentScheduleWithCorrections
  :: Day -> Cycle -> Day -> ScheduleConfig -> ShiftedSchedule
generateRecurrentScheduleWithCorrections anchorDate cycle endDate ScheduleConfig {..}
  = generateRecurrentSchedule cycle anchorDate endDate &
      ((correction cycle anchorDate endDate) >>>
      (fmap $ applyEOMC anchorDate cycle (fromJust eomc)) >>>
      (fmap $ applyBDC (fromJust bdc) (fromJust calendar)))

plusCycle :: Day -> Cycle -> Day
plusCycle date cycle = shiftDate date (n cycle) (p cycle)

minusCycle :: Day -> Cycle -> Day
minusCycle date cycle = shiftDate date (-n cycle) (p cycle)

shiftDate :: Day -> Integer -> Period -> Day
shiftDate date n p = case p of
  P_D -> addDays n date
  P_W -> addDays (n * 7) date
  P_M -> addGregorianMonthsClip n date
  P_Q -> addGregorianMonthsClip (n * 3) date
  P_H -> addGregorianMonthsClip (n * 6) date
  P_Y -> addGregorianYearsClip n date


{- End of Month Convention -}
applyEOMC :: Day -> Cycle -> EOMC -> Day -> Day
applyEOMC s Cycle {..} endOfMonthConvention date
  | isLastDayOfMonthWithLessThan31Days s
    && p /= P_D
    && p /= P_W
    && endOfMonthConvention == EOMC_EOM
  = moveToEndOfMonth date
  | otherwise
  = date

isLastDayOfMonthWithLessThan31Days :: Day -> Bool
isLastDayOfMonthWithLessThan31Days date =
  let (day, month, year) = toGregorian date
      isLastDay = gregorianMonthLength (toInteger year) month == fromInteger day
  in  day <  31 && isLastDay

moveToEndOfMonth :: Day -> Day
moveToEndOfMonth date =
  let (_, month, year) = toGregorian date
      monthLength      = gregorianMonthLength (toInteger year) month
  in  fromGregorian (toInteger year) month monthLength
