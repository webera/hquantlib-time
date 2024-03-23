module QuantLib.Time.Calendars.German
  ( module QuantLib.Time.Calendars.German,
  )
where

import QuantLib.Time.Date (Holiday(..), isWeekEnd)

import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Calendar.Easter (gregorianEaster)

data GermanMarket = Settlement | FrankfurtStockExchange | Xetra | Eurex | Euwax

newtype GermanCalendar = GermanCalendar GermanMarket

instance Holiday GermanCalendar where
  isHoliday (GermanCalendar Eurex) (_, 12, 31) = True
  isHoliday (GermanCalendar Euwax) (y, m, d) = whitMonday
    where
      whitMonday = fromGregorian y m d == addDays 50 (gregorianEaster y)

  isHoliday (GermanCalendar _) (_, 1, 1) = True
  isHoliday (GermanCalendar _) (_, 5, 1) = True
  isHoliday (GermanCalendar _) (_, 12, 24) = True
  isHoliday (GermanCalendar _) (_, 12, 25) = True
  isHoliday (GermanCalendar _) (_, 12, 26) = True

  isHoliday _ (y, m, d) = not (weekend || easter)
    where
      weekend = isWeekEnd day
      easter = day == addDays 1 (gregorianEaster y)
      day = fromGregorian y m d
