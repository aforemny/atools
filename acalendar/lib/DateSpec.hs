{-# LANGUAGE RecordWildCards #-}

module DateSpec where

import Data.List (intercalate)
import Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as Calendar
import Data.Time.Format.ISO8601 (iso8601Show)
import Weekday (Weekday (..))
import qualified Weekday
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

toString :: DateSpec -> String
toString (DateSpec {..}) =
  intercalate
    " "
    ( concat
        [ map show days
            ++ map
              ( \n -> case n of
                  1 -> "Jan"
                  2 -> "Feb"
                  3 -> "Mar"
                  4 -> "Apr"
                  5 -> "May"
                  6 -> "Jun"
                  7 -> "Jul"
                  8 -> "Aug"
                  9 -> "Sep"
                  10 -> "Oct"
                  11 -> "Nov"
                  12 -> "Dec"
              )
              months
            ++ map show years
            ++ map Weekday.toString weekdays
            ++ maybe [] ((: []) . (++) "-" . show) back
            ++ maybe [] ((: []) . ((++) "FROM ") . iso8601Show) from
            ++ maybe [] ((: []) . ((++) "UNTIL ") . iso8601Show) until
        ]
    )

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

-- TODO garbage
occurrenceAfter :: Day -> DateSpec -> Maybe Day
occurrenceAfter date dateSpec =
  case filter
    (\day -> satisfies day dateSpec)
    ([toEnum (fromEnum date + n) | n <- [0 .. 2 * 360]]) of
    [] -> Nothing
    (x : _) -> Just x
