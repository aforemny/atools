{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (forM, forM_)
import qualified Data.ByteString.Lazy as ByteString
import Data.Default (Default (def))
import Data.Function ((&))
import Data.List (sortBy)
import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Data.Time.Calendar (Day (..), diffDays)
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as Format
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Time.Zones
import Event (Event)
import qualified Event
import qualified File
import Network.HTTP.Simple (getResponseBody, httpLBS)
import Network.Protocol.HTTP.DAV
import Network.URI (URI)
import qualified Network.URI as Uri
import Options.Applicative
import qualified Storage as S
import qualified System.Console.ANSI as Ansi
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((<.>), (</>))
import qualified Text.Blaze as B
import qualified Text.Blaze.Renderer.Utf8 as B
import Text.ICalendar.Parser (parseICalendar)
import Text.ICalendar.Printer (printICalendar)
import Text.ICalendar.Types
import Text.Printf (printf)
import qualified Text.XML as Xml
import qualified Text.XML.Cursor as Xml
import TimeSpec (TimeSpec (TimeSpec))
import qualified TimeSpec
import Prelude hiding (readFile)

data Args
  = EditArgs {includes :: [FilePath], filePath :: FilePath}
  | ExportArgs {includes :: [FilePath], output :: FilePath}
  | ListArgs {includes :: [FilePath], urls :: [URI]}
  | SyncArgs {includes :: [FilePath], url :: URI}

editArgs, exportArgs, listArgs, syncArgs :: Parser Args
editArgs = EditArgs <$> includesArg <*> fileArg
exportArgs = ExportArgs <$> includesArg <*> outputArg
listArgs = ListArgs <$> includesArg <*> many urlArg
syncArgs = SyncArgs <$> includesArg <*> urlArg

editCmd, exportCmd, listCmd, syncCmd :: Mod CommandFields Args
editCmd = command "edit" (info editArgs fullDesc)
exportCmd = command "export" (info exportArgs fullDesc)
listCmd = command "list" (info listArgs fullDesc)
syncCmd = command "sync" (info syncArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (editCmd <> exportCmd <> listCmd <> syncCmd) <**> helper) idm

includesArg :: Parser [FilePath]
includesArg = many (strOption (long "include" <> short 'I' <> metavar "DIR"))

fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE" <> completer fileArgComp)

fileArgComp :: Completer
fileArgComp =
  mkCompleter $ \_ -> do
    includes <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
    fmap (map S.toString) . S.listFiles =<< S.open includes

outputArg :: Parser FilePath
outputArg =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "DIR"
        <> help "Export directory"
        <> action "directory"
    )

urlArg :: Parser URI
urlArg =
  option
    (maybeReader Uri.parseAbsoluteURI)
    ( long "url"
        <> short 'u'
        <> metavar "URL"
        <> help "Export URL"
    )

data E = E (Maybe (Day, Maybe TimeOfDay)) (Maybe Text) deriving (Show)

main :: IO ()
main = do
  args <- execParser mainArgs
  curDay <- Clock.utctDay <$> Clock.getCurrentTime
  includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
  case args of
    ListArgs {..} -> do
      ltz <- loadLocalTZ
      cals <-
        concat
          <$> ( forM urls $ \url -> do
                  -- TODO Right (_, ical) <- evalDAVT url getContentM
                  ical <- getResponseBody <$> httpLBS (fromString (show url))
                  case parseICalendar def (show url) ical of
                    Left e -> die e
                    Right (cals, ws) -> do
                      mapM_ (printf "warning: %s\n") ws
                      pure cals
              )
      let ves = concatMap (Map.toList . vcEvents) cals
      let ds = [toEnum (fromEnum curDay + n) :: Day | n <- [0 .. 1]]
      forM_ ds $ \d -> do
        printf "%s\n" (show d)
        es' <-
          concat
            <$> ( forM ves $ \((uid, rid), e) -> do
                    let dateTimeToStart dateTime@(FloatingDateTime {}) = do
                          pure $
                            ( localDay (dateTimeFloating dateTime),
                              Just $ localTimeOfDay (dateTimeFloating dateTime)
                            )
                        dateTimeToStart dateTime@(UTCDateTime {}) = do
                          let localTime = utcToLocalTimeTZ ltz (dateTimeUTC dateTime)
                          pure $
                            ( localDay localTime,
                              Just $ localTimeOfDay localTime
                            )
                        dateTimeToStart dateTime@(ZonedDateTime {}) = do
                          etz <- loadSystemTZ (LazyText.unpack (dateTimeZone dateTime))
                          let utcTime = localTimeToUTCTZ etz (dateTimeFloating dateTime)
                              localTime = utcToLocalTimeTZ ltz utcTime
                          pure $
                            ( localDay localTime,
                              Just $ localTimeOfDay localTime
                            )
                    case (Set.toList (veRRule e), veRecurId e) of
                      (rrs@(_ : _), Nothing) -> do
                        start' <-
                          maybe
                            (pure Nothing)
                            ( \dtStart ->
                                Just <$> case dtStart of
                                  (DTStartDateTime {}) ->
                                    dateTimeToStart (dtStartDateTimeValue dtStart)
                                  (DTStartDate {}) ->
                                    pure (dateValue (dtStartDateValue dtStart), Nothing)
                            )
                            (veDTStart e)
                        case start' of
                          Just start@(startDate, startTimeOfDay) -> do
                            catMaybes
                              <$> ( forM (map rRuleValue rrs) $ \rr -> do
                                      let untilSatisfies (Just (Left (Left date))) =
                                            pure $ d <= dateValue date
                                          untilSatisfies (Just (Left (Right dateTime))) = do
                                            (startDate, _) <- dateTimeToStart dateTime
                                            pure $ d <= startDate
                                          untilSatisfies Nothing = pure True
                                      if
                                          | (recurFreq rr == Weekly),
                                            (diffDays startDate d `mod` 7 == 0) ->
                                              do
                                                satisfies <- untilSatisfies (recurUntilCount rr)
                                                if satisfies
                                                  then pure (Just (E (Just (d, startTimeOfDay)) (LazyText.toStrict . summaryValue <$> veSummary e)))
                                                  else pure Nothing
                                          | recurFreq rr == Weekly -> pure Nothing
                                          | otherwise -> error "unknown RRULE"
                                  )
                          Nothing -> error "RRULE without DTSTART"
                      ([], Just recurrenceId@(RecurrenceIdDateTime {})) -> do
                        start@(startDate, _) <- dateTimeToStart (recurrenceIdDateTime recurrenceId)
                        if d == startDate
                          then pure [E (Just start) (LazyText.toStrict . summaryValue <$> veSummary e)]
                          else pure []
                      ([], Nothing) -> do
                        start <-
                          maybe
                            (pure Nothing)
                            ( \dtStart ->
                                Just <$> case dtStart of
                                  (DTStartDateTime {}) ->
                                    dateTimeToStart (dtStartDateTimeValue dtStart)
                                  (DTStartDate {}) ->
                                    pure (dateValue (dtStartDateValue dtStart), Nothing)
                            )
                            (veDTStart e)
                        end <-
                          maybe
                            (pure Nothing)
                            ( \dtEndDuration ->
                                Just <$> case dtEndDuration of
                                  Left dtEnd ->
                                    case dtEnd of
                                      DTEndDateTime {} ->
                                        dateTimeToStart (dtEndDateTimeValue dtEnd)
                                      DTEndDate {} ->
                                        pure (dateValue (dtEndDateValue dtEnd), Nothing)
                                  Right durationProp -> error "DTEND duration not supported"
                            )
                            (veDTEndDuration e)
                        case (start, end) of
                          (Just start@(startDay, startTimeOfDay), Just end@(endDay, _)) ->
                            if startDay <= d && d < endDay
                              then pure [E (Just (d, startTimeOfDay)) (LazyText.toStrict . summaryValue <$> veSummary e)]
                              else pure []
                          _ -> error "DTSTART XOR DTEND"
                      _ -> error "both RRULE and RECURID present"
                )
        let es =
              es'
                & sortBy
                  ( comparing
                      ( \(E start _) ->
                          case start of
                            Nothing ->
                              (Nothing, Nothing)
                            Just (startDay, Nothing) ->
                              (Just startDay, Nothing)
                            Just (startDay, Just startTimeOfDay) ->
                              (Just startDay, Just startTimeOfDay)
                      )
                  )
        forM_ es $ \(E start summary') -> do
          let summary = fromMaybe "[no summary]" summary'
          case start of
            Nothing -> printf "%s\n" summary
            Just (startDay, Nothing) -> printf "%s %s\n" (show startDay) summary
            Just (startDay, Just startTimeOfDay) -> printf "%s %s %s\n" (show startDay) (show startTimeOfDay) summary

readFile :: S.Handle -> S.Entry -> IO File.File
readFile h e = do
  S.readFile h e >>= \case
    Nothing ->
      -- TODO io exception
      die "does not exist"
    Just s -> case File.parse (S.toString e) (Text.pack s) of
      Left es ->
        die es
      Right f ->
        return f

readEvents :: S.Handle -> S.Entry -> IO [Event]
readEvents h e = do
  File.events <$> readFile h e

die :: Show a => a -> IO b
die s = do
  print s
  exitWith (ExitFailure 1)
