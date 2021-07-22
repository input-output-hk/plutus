{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.Utility.DateShift
  ( applyBDC
  , applyBDCWithCfg
  )
where

import           Data.Maybe                                       (fromJust)
import           Data.Time                                        (Day, addDays, toGregorian)
import           Data.Time.Calendar.WeekDate                      (toWeekDate)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms (BDC (..), Calendar (..), ScheduleConfig (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule      (ShiftedDay (..))

{- Business Day Convention -}

applyBDCWithCfg :: ScheduleConfig -> Day -> ShiftedDay
applyBDCWithCfg ScheduleConfig {..} = applyBDC (fromJust bdc) (fromJust calendar)

applyBDC :: BDC -> Calendar -> Day -> ShiftedDay
applyBDC BDC_NULL _ date =
  ShiftedDay { paymentDay = date, calculationDay = date }

applyBDC BDC_SCF calendar date = ShiftedDay
  { paymentDay     = getFollowingBusinessDay date calendar
  , calculationDay = getFollowingBusinessDay date calendar
  }

applyBDC BDC_SCMF calendar date = ShiftedDay
  { paymentDay     = shiftModifiedFollowing date calendar
  , calculationDay = shiftModifiedFollowing date calendar
  }

applyBDC BDC_CSF calendar date = ShiftedDay
  { paymentDay     = getFollowingBusinessDay date calendar
  , calculationDay = date
  }

applyBDC BDC_CSMF calendar date = ShiftedDay
  { paymentDay     = shiftModifiedFollowing date calendar
  , calculationDay = date
  }

applyBDC BDC_SCP calendar date = ShiftedDay
  { paymentDay     = getPreceedingBusinessDay date calendar
  , calculationDay = getPreceedingBusinessDay date calendar
  }

applyBDC BDC_SCMP calendar date = ShiftedDay
  { paymentDay     = shiftModifiedPreceeding date calendar
  , calculationDay = shiftModifiedPreceeding date calendar
  }

applyBDC BDC_CSP calendar date = ShiftedDay
  { paymentDay     = getPreceedingBusinessDay date calendar
  , calculationDay = date
  }

applyBDC BDC_CSMP calendar date = ShiftedDay
  { paymentDay     = shiftModifiedPreceeding date calendar
  , calculationDay = date
  }


shiftModifiedFollowing :: Day -> Calendar -> Day
shiftModifiedFollowing date calendar =
  let (_, month, _)        = toGregorian date
      shiftedFollowing     = getFollowingBusinessDay date calendar
      (_, shiftedMonth, _) = toGregorian shiftedFollowing
  in  if month == shiftedMonth
        then shiftedFollowing
        else getPreceedingBusinessDay date calendar

shiftModifiedPreceeding :: Day -> Calendar -> Day
shiftModifiedPreceeding date calendar =
  let (_, month, _)        = toGregorian date
      shiftedPreceeding    = getPreceedingBusinessDay date calendar
      (_, shiftedMonth, _) = toGregorian shiftedPreceeding
  in  if month == shiftedMonth
        then shiftedPreceeding
        else getFollowingBusinessDay date calendar

getFollowingBusinessDay :: Day -> Calendar -> Day
getFollowingBusinessDay date calendarType =
  case calendarType of
    CLDR_MF ->
      case toWeekDate date of
        (_, _, 6) ->
          addDays 2 date
        (_, _, 7) ->
          addDays 1 date
        _ ->
          date
    CLDR_NC ->
      date

getPreceedingBusinessDay :: Day -> Calendar -> Day
getPreceedingBusinessDay date calendarType =
  case calendarType of
    CLDR_MF ->
      case toWeekDate date of
        (_, _, 6) ->
          addDays (-1) date
        (_, _, 7) ->
          addDays (-2) date
        _ ->
          date
    CLDR_NC ->
      date
