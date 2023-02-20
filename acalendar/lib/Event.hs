{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Event (Event (..), toString, satisfies, toICalendar, tags
) where

import Data.Default (Default (def))
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time.Calendar (Day (ModifiedJulianDay), addDays)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (LocalTime), timeToTimeOfDay)
import DateSpec (DateSpec)
import qualified DateSpec
import Text.ICalendar.Types
import TimeSpec (TimeSpec (TimeSpec))
import qualified TimeSpec
import Prelude hiding (until)

data Event = Event
  { dateSpec :: DateSpec,
    timeSpec :: Maybe TimeSpec,
    title :: Text
  }
  deriving (Show)

tags :: Event -> Map Text Text
tags e =
  title e
    & Text.words
    & filter (Text.isInfixOf ":")
    & map (Text.breakOn ":")
    & Map.fromList

satisfies :: Day -> Event -> Bool
satisfies day = DateSpec.satisfies day . dateSpec

toString :: Event -> String
toString (Event {..}) =
  intercalate
    " "
    ( filter
        (not . null)
        [ DateSpec.toString dateSpec,
          maybe "" TimeSpec.toString timeSpec,
          Text.unpack title
        ]
    )

toICalendar :: Day -> Int -> Event -> VCalendar
toICalendar day n e =
  let uid =
        LazyText.pack
          ( concat
              [ formatTime defaultTimeLocale "%F" day,
                "+",
                show n,
                "@TODO"
              ]
          )
      defVEvent =
        ( VEvent
            { veDTStamp = DTStamp (UTCTime (ModifiedJulianDay 0) 0) def,
              veUID = UID uid def,
              veClass = def,
              veDTStart = def,
              veCreated = def,
              veDescription = def,
              veGeo = def,
              veLastMod = def,
              veLocation = def,
              veOrganizer = def,
              vePriority = def,
              veSeq = def,
              veStatus = def,
              veSummary = def,
              veTransp = def,
              veUrl = def,
              veRecurId = def,
              veRRule = def,
              veDTEndDuration = def,
              veAttach = def,
              veAttendee = def,
              veCategories = def,
              veComment = def,
              veContact = def,
              veExDate = def,
              veRStatus = def,
              veRelated = def,
              veResources = def,
              veRDate = def,
              veAlarms = def,
              veOther = def
            }
        )
      vEvent =
        defVEvent
          { veSummary =
              Just
                Summary
                  { summaryValue = LazyText.fromStrict (title e),
                    summaryAltRep = Nothing,
                    summaryLanguage = Nothing,
                    summaryOther = def
                  },
            veDTStart =
              Just
                ( case Event.timeSpec e of
                    Just (TimeSpec dt _) -> DTStartDateTime (FloatingDateTime (LocalTime day (timeToTimeOfDay dt))) def
                    _ -> DTStartDate (Date day) def
                ),
            veDTEndDuration =
              Just
                ( Left
                    ( case Event.timeSpec e of
                        Just (TimeSpec _ dt) -> DTEndDateTime (FloatingDateTime (LocalTime day (timeToTimeOfDay dt))) def
                        _ -> DTEndDate (Date (addDays 1 day)) def
                    )
                )
          }
   in def {vcEvents = Map.singleton (uid, Nothing) vEvent}
