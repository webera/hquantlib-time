{-# LANGUAGE InstanceSigs #-}
module QuantLib.Time.Calendars.UnitedStates
  ( module QuantLib.Time.Calendars.UnitedStates,
  )
where

import QuantLib.Time.Date (Holiday(..), isWeekEnd)

import Data.Time.Calendar (addDays, dayOfWeek, toGregorian, fromGregorian, Day, DayOfWeek(..), Year)
import Data.Time.Calendar.Easter (gregorianEaster)

data UnitedStatesMarket = Settlement | NYSE | GovernmentBond | NERC | LiborImpact | FederalReserve | SOFR

newtype UnitedStatesCalendar = UnitedStatesCalendar UnitedStatesMarket

-- TODO Presidential election day, first Tuesday in November of election years (until 1980)
instance Holiday UnitedStatesCalendar where
  isHoliday :: UnitedStatesCalendar -> (Integer, Int, Int) -> Bool
  isHoliday (UnitedStatesCalendar _) (_, 1, 1) = True   -- New Year's Day, January 1st (possibly moved to Monday if actually on Sunday)
  isHoliday (UnitedStatesCalendar NYSE) (y, 1, d) = y > 1998 && fromGregorian y 1 d == martinLutherDay y -- third Monday in January (since 1998)
  isHoliday (UnitedStatesCalendar _) (y, 1, d) = y > 1983 && fromGregorian y 1 d == martinLutherDay y    -- third Monday in January (since 1983)
  isHoliday (UnitedStatesCalendar _) (y, 2, d) = fromGregorian y 2 d == presidentsDay y                  -- Presidents' Day third Monday in February
  isHoliday (UnitedStatesCalendar _) (y, 5, d) = fromGregorian y 5 d == memorialDay y
  isHoliday (UnitedStatesCalendar _) (y, 7, d) = isIndependanceHoliday (fromGregorian y 7 d)             -- Independance days holiday
    where
      independanceDay = fromGregorian y 7 4
      isIndependanceHoliday day = day == case dayOfWeek independanceDay of
                                           Sunday -> addDays 1 independanceDay      -- moved to Monday if Sunday
                                           Saturday -> addDays (-1) independanceDay -- moved to Friday if Saturday
                                           _ -> independanceDay
  isHoliday (UnitedStatesCalendar _) (y, 9, d) = fromGregorian y 9 d == laborDay y
  isHoliday (UnitedStatesCalendar _) (y, 11, d) = fromGregorian y 11 d == thanksgivingDay y           -- Thanksgiving day
  isHoliday (UnitedStatesCalendar _) (y, 12, d) = isChristmasHoliday (fromGregorian y 12 d)           -- Christmas day
    where
      christmasDay = fromGregorian y 12 25
      isChristmasHoliday day = day == case dayOfWeek christmasDay of
                                        Sunday -> addDays 1 christmasDay            -- moved to Monday if Sunday
                                        Saturday -> addDays (-1) christmasDay       -- moved to Friday if Saturday
                                        _ -> christmasDay
  isHoliday (UnitedStatesCalendar _) (y, m, d) = not (weekend || isGoodFriday)
    where
      weekend = isWeekEnd day
      isGoodFriday = day == addDays (-2) (gregorianEaster y)
      day = fromGregorian y m d

-- thrid monday of jan
martinLutherDay :: Year -> Day
martinLutherDay year =
  let firstDayOfJan = fromGregorian year 1 1
      firstMondayOfJan = head $ dropWhile (\x -> dayOfWeek x /= Monday) $ firstDayOfJan : iterate (addDays 1) firstDayOfJan
  in addDays 14 firstMondayOfJan

-- third monday of feb
presidentsDay :: Year -> Day
presidentsDay year =
  let firstDayOfFeb = fromGregorian year 2 1
      nextMonday day = head $ dropWhile (\x -> dayOfWeek x /= Monday) $ day : iterate (addDays 1) day
  in addDays 14 $ nextMonday firstDayOfFeb

-- last Monday in May 
memorialDay :: Year -> Day
memorialDay year =
  let lastDayOfMay = fromGregorian year 5 31
  in head $ dropWhile (\x -> dayOfWeek x /= Monday) $ lastDayOfMay : iterate (addDays (-1)) lastDayOfMay

-- first Monday in September
laborDay :: Year -> Day
laborDay year =
  let firstDayOfSep = fromGregorian year 9 1
  in head $ dropWhile (\x -> dayOfWeek x /= Monday) $ firstDayOfSep : iterate (addDays (-1)) firstDayOfSep

-- fourth Thursday in November
thanksgivingDay :: Year -> Day
thanksgivingDay year =
  let firstDayOfNov = fromGregorian year 11 1
      firstThursdayOfNov = head $ dropWhile (\x -> dayOfWeek x /= Thursday) $ firstDayOfNov : iterate (addDays 1) firstDayOfNov
  in addDays 21 firstThursdayOfNov
