module DateSpec where

import Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as Calendar
import Weekday (Weekday (..))
import Prelude hiding (until)

data DateSpec = DateSpec
  { days :: [Int],
    months :: [Int],
    years :: [Int],
    weekdays :: [Weekday],
    back :: Maybe Int,
    from :: Maybe Day,
    until :: Maybe Day
  }
  deriving (Show, Eq)

empty :: DateSpec
empty =
  DateSpec
    { days = [],
      months = [],
      years = [],
      weekdays = [],
      back = Nothing,
      from = Nothing,
      until = Nothing
    }

satisfies :: Day -> DateSpec -> Bool
satisfies date dateSpec =
  let (dYear, dMonth, dDay) =
        Calendar.toGregorian date
      dWeekday =
        case Calendar.dayOfWeek date of
          Calendar.Monday -> Mon
          Calendar.Tuesday -> Tue
          Calendar.Wednesday -> Wed
          Calendar.Thursday -> Thu
          Calendar.Friday -> Fri
          Calendar.Saturday -> Sat
          Calendar.Sunday -> Sun
   in case back dateSpec of
        Just n ->
          satisfies (toEnum (fromEnum date + n)) dateSpec {back = Nothing}
        Nothing ->
          if weekdays dateSpec /= [] && days dateSpec /= []
            then
              and
                [ any
                    (\date -> satisfies date (dateSpec {weekdays = []}))
                    [toEnum (fromEnum date - n) | n <- [0 .. 6]],
                  elem dWeekday (weekdays dateSpec),
                  all
                    (\date -> not (satisfies date dateSpec))
                    [toEnum (fromEnum date - n) | n <- [1 .. 6]]
                ]
            else
              and
                [ maybe True (<= date) (from dateSpec),
                  maybe True (date <=) (until dateSpec),
                  case days dateSpec of
                    [] -> True
                    _ -> elem dDay (days dateSpec),
                  case months dateSpec of
                    [] -> True
                    _ -> elem dMonth (months dateSpec),
                  case years dateSpec of
                    [] -> True
                    _ -> elem (fromIntegral dYear) (years dateSpec),
                  case weekdays dateSpec of
                    [] -> True
                    _ -> elem dWeekday (weekdays dateSpec)
                ]
