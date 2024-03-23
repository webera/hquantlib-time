module QuantLib.Time.Date
  ( module QuantLib.Time.Date,
  )
where

import Data.Time.Calendar (Day (..), DayOfWeek (..), addDays, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

-- | Business Day conventions
-- - These conventions specify the algorithm used to adjust a date in case it is not a valid business day.
data BusinessDayConvention
  = Following
  | ModifiedFollowing
  | Preceding
  | ModifiedPreceding
  | Unadjusted
  deriving (Show, Eq, Enum)

-- -- | Week days
-- data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
--         deriving (Show, Eq, Enum)

-- | Date
type Date = Day


{-
    inline bool Calendar::isBusinessDay(const Date& d) const {
        QL_REQUIRE(impl_, "no calendar implementation provided");

#ifdef QL_HIGH_RESOLUTION_DATE
        const Date _d(d.dayOfMonth(), d.month(), d.year());
#else
        const Date& _d = d;
#endif

        if (!impl_->addedHolidays.empty() &&
            impl_->addedHolidays.find(_d) != impl_->addedHolidays.end())
            return false;

        if (!impl_->removedHolidays.empty() &&
            impl_->removedHolidays.find(_d) != impl_->removedHolidays.end())
            return true;

        return impl_->isBusinessDay(_d);
    }

    bool Germany::FrankfurtStockExchangeImpl::isBusinessDay(
      const Date& date) const {
        Weekday w = date.weekday();
        Day d = date.dayOfMonth(), dd = date.dayOfYear();
        Month m = date.month();
        Year y = date.year();
        Day em = easterMonday(y);
        if (isWeekend(w)
            // New Year's Day
            || (d == 1 && m == January)
            // Good Friday
            || (dd == em-3)
            // Easter Monday
            || (dd == em)
            // Labour Day
            || (d == 1 && m == May)
            // Christmas' Eve
            || (d == 24 && m == December)
            // Christmas
            || (d == 25 && m == December)
            // Christmas Day
            || (d == 26 && m == December))
            return false; // NOLINT(readability-simplify-boolean-expr)
        return true;
    }
-}

-- | Defines a holidays for given calendar. Corresponds to calendar class in QuantLib
class Holiday m where
  isHoliday :: m -> (Integer, Int, Int) -> Bool

  isBusinessDay :: m -> Date -> Bool
  isBusinessDay m d = not (isHoliday m $ toGregorian d)

  hBusinessDayBetween :: m -> (Date, Date) -> Int
  hBusinessDayBetween m (fd, td) = foldl countDays 0 listOfDates
    where
      countDays counter x = counter + fromEnum (isBusinessDay m x)
      listOfDates = getDaysBetween (fd, td)

-- | Gets a week day
getWeekDay :: Date -> DayOfWeek
getWeekDay d = toEnum (weekDay - 1)
  where
    (_, _, weekDay) = toWeekDate d

-- | Generate a list of all dates inbetween
getDaysBetween :: (Day, Day) -> [Day]
getDaysBetween (fd, td) = reverse $ generator fd []
  where
    generator date x
      | date < td = generator nextDate (nextDate : x)
      | otherwise = x
      where
        nextDate = addDays 1 date

-- | Checks if the day is a weekend, i.e. Saturday or Sunday
isWeekEnd :: Date -> Bool
isWeekEnd d = (weekday == Saturday) || (weekday == Sunday)
  where
    weekday = getWeekDay d

-- | Gets the next working day
getNextBusinessDay :: Holiday a => a -> Date -> Date
getNextBusinessDay m d
  | isBusinessDay m nextDay = nextDay
  | otherwise = getNextBusinessDay m nextDay
  where
    nextDay = addDays 1 d
