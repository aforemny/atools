{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Default (Default (def))
import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as Format
import Event (Event)
import qualified Event
import qualified File
import Network.Protocol.HTTP.DAV
import Network.URI (URI)
import qualified Network.URI as Uri
import Options.Applicative
import qualified Storage as S
import qualified System.Console.ANSI as Ansi
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((<.>), (</>))
import Text.ICalendar.Printer (printICalendar)
import Text.ICalendar.Types
import qualified Text.XML.Cursor as Xml
import TimeSpec (TimeSpec (TimeSpec))
import qualified TimeSpec
import Prelude hiding (readFile)

data Args
  = EditArgs {includes :: [FilePath], filePath :: FilePath}
  | ExportArgs {includes :: [FilePath], output :: FilePath}
  | ListArgs {includes :: [FilePath]}
  | SyncArgs {includes :: [FilePath], url :: URI}

editArgs, exportArgs, listArgs, syncArgs :: Parser Args
editArgs = EditArgs <$> includesArg <*> fileArg
exportArgs = ExportArgs <$> includesArg <*> outputArg
listArgs = ListArgs <$> includesArg
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

main :: IO ()
main = do
  args <- execParser mainArgs
  curDay <- Clock.utctDay <$> Clock.getCurrentTime
  includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
  case args of
    EditArgs {..} -> do
      h <- S.open (includes' ++ includes)
      let e = S.fromString filePath
      f <- readFile h e
      _ <- S.writeFile h e (File.write (File.prune curDay f))
      S.editFile h e
    ListArgs {..} -> do
      h <- S.open (includes' ++ includes)
      es <-
        (fmap concat . mapM (readEvents h))
          =<< ( filter ((==) "calendar/reminders" . S.filePath)
                  <$> S.listFiles h
              )
      mapM_
        ( \(day, es) -> do
            Ansi.setSGR [Ansi.SetConsoleIntensity Ansi.BoldIntensity]
            putStrLn (Format.formatTime Format.defaultTimeLocale "%A, %F" day)
            Ansi.setSGR [Ansi.SetConsoleIntensity Ansi.NormalIntensity]
            mapM_
              ( \e -> Text.putStrLn $ case Event.timeSpec e of
                  Just timeSpec ->
                    Text.concat
                      [ TimeSpec.toText timeSpec,
                        " ",
                        Event.title e
                      ]
                  Nothing ->
                    Event.title e
              )
              es
        )
        ( catMaybes $
            map
              ( \day -> case filter (Event.satisfies day) es of
                  [] -> Nothing
                  es ->
                    Just
                      ( day,
                        List.sortOn
                          ( \e ->
                              case Event.timeSpec e of
                                Just (TimeSpec start end) ->
                                  (1 :: Int, start, -end, Event.title e)
                                Nothing ->
                                  (0, 0, 0, Event.title e)
                          )
                          es
                      )
              )
              [toEnum (fromEnum curDay + n) | n <- [0 .. 1]]
        )
    ExportArgs {..} -> do
      h <- S.open (includes' ++ includes)
      es <-
        (fmap concat . mapM (readEvents h))
          =<< ( filter ((==) "calendar/reminders" . S.filePath)
                  <$> S.listFiles h
              )
      mapM_
        ( \ical ->
            let uid = fst (head (Map.keys (vcEvents ical)))
                fp = output </> LazyText.unpack uid <.> "ics"
             in ByteString.writeFile fp (printICalendar def ical)
        )
        $ map (\(d, (n, e)) -> Event.toICalendar d n e) $
          concatMap
            (\d -> map (d,) (zip [1 ..] (filter (Event.satisfies d) es)))
            ([toEnum (fromEnum curDay + n) | n <- [0 .. 90]])
    SyncArgs {..} -> do
      h <- S.open (includes' ++ includes)
      es <-
        (fmap concat . mapM (readEvents h))
          =<< ( filter ((==) "calendar/reminders" . S.filePath)
                  <$> S.listFiles h
              )
      Right fs <-
        evalDAVT (show url) $
          concat
            . ( \doc ->
                  Xml.fromDocument doc
                    Xml.$// Xml.element "{DAV:}response"
                      Xml.&| ( Xml.checkNode
                                 ( \node ->
                                     Xml.fromNode node
                                       Xml.$// Xml.element "{urn:ietf:params:xml:ns:caldav}calendar-data"
                                 )
                             )
                      Xml.&/ Xml.element "{DAV:}href"
                      Xml.&/ Xml.content
              )
            <$> caldavReportM
      mapM_
        ( \f -> do
            putStrLn ("- " ++ Text.unpack f)
            evalDAVT
              (show (url {Uri.uriPath = Text.unpack f}))
              delContentM
        )
        fs
      mapM_
        ( \ical -> do
            let uid = fst (head (Map.keys (vcEvents ical)))
                fp = LazyText.unpack uid <.> "ics"
            putStrLn ("+ " ++ fp)
            evalDAVT (show (url {Uri.uriPath = Uri.uriPath url </> fp})) $
              putContentM (Just "text/calendar", printICalendar def ical)
        )
        $ map (\(d, (n, e)) -> Event.toICalendar d n e) $
          concatMap
            (\d -> map (d,) (zip [1 ..] (filter (Event.satisfies d) es)))
            ([toEnum (fromEnum curDay + n) | n <- [0 .. 90]])

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
