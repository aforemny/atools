{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (some, (<**>))
import qualified Data.ByteString.Lazy as ByteString
import Data.Default (Default (def))
import Data.List (nub, sort)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time.Calendar (Day)
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as Format
import Event (Event)
import qualified Event
import qualified File
import Network.Protocol.HTTP.DAV
import Network.URI (URI)
import qualified Network.URI as Uri
import Options.Applicative (CommandFields, Completer, Mod, Parser, ParserInfo, action, bashCompleter, command, completer, execParser, fullDesc, help, helper, hsubparser, idm, info, long, maybeReader, metavar, option, short, strArgument, strOption)
import qualified System.Console.ANSI as Ansi
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((<.>), (</>))
import Text.ICalendar.Printer (printICalendar)
import Text.ICalendar.Types
import qualified Text.XML.Cursor as Xml
import TimeSpec (TimeSpec (TimeSpec))
import qualified TimeSpec

data Args
  = ListArgs
      { sources :: [FilePath]
      }
  | ExportArgs
      { sources :: [FilePath],
        output :: FilePath
      }
  | SyncArgs
      { sources :: [FilePath],
        url :: URI
      }

exportArgs, listArgs, syncArgs :: Parser Args
exportArgs = ExportArgs <$> sourcesArg <*> outputArg
listArgs = ListArgs <$> sourcesArg
syncArgs = SyncArgs <$> sourcesArg <*> urlArg

exportCmd, listCmd, syncCmd :: Mod CommandFields Args
exportCmd = command "export" (info exportArgs fullDesc)
listCmd = command "list" (info listArgs fullDesc)
syncCmd = command "sync" (info syncArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (listCmd <> exportCmd <> syncCmd) <**> helper) idm

sourcesArg :: Parser [FilePath]
sourcesArg =
  some
    ( strArgument
        ( metavar "SOURCE"
            <> help "File or directory to read calendar entries from"
            <> completer sourceCompleter
        )
    )

sourceCompleter :: Completer
sourceCompleter =
  bashCompleter "directory" <> bashCompleter "file"

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
  filePaths <- collectSources (sources args)
  events <- concat <$> mapM readEvents filePaths
  case args of
    ListArgs _ -> do
      let displayDay :: Day -> [Event] -> [Event]
          displayDay day es = filter (Event.satisfies day) es
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
              ( \day -> case displayDay day events of
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
      mapM_
        ( \ical ->
            let uid = fst (head (Map.keys (vcEvents ical)))
                fp = output </> LazyText.unpack uid <.> "ics"
             in ByteString.writeFile fp (printICalendar def ical)
        )
        $ map (\(d, (n, e)) -> Event.toICalendar d n e) $
          concatMap
            (\d -> map (d,) (zip [1 ..] (filter (Event.satisfies d) events)))
            ([toEnum (fromEnum curDay + n) | n <- [0 .. 90]])
    SyncArgs {..} -> do
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
            (\d -> map (d,) (zip [1 ..] (filter (Event.satisfies d) events)))
            ([toEnum (fromEnum curDay + n) | n <- [0 .. 90]])

collectSources :: [FilePath] -> IO [FilePath]
collectSources filePaths =
  let go :: FilePath -> IO [FilePath]
      go filePath = do
        isFile <- doesFileExist filePath
        if isFile
          then return (if List.isPrefixOf "." filePath then [] else [filePath])
          else do
            isDirectory <- doesDirectoryExist filePath
            if isDirectory
              then
                fmap concat . mapM go
                  =<< ( map (filePath </>)
                          <$> filter (not . List.isPrefixOf ".")
                          <$> listDirectory filePath
                      )
              else return []
   in sort . nub . concat <$> mapM go filePaths

readEvents :: FilePath -> IO [Event]
readEvents filePath =
  File.readFile filePath >>= \case
    Left es ->
      die es
    Right file ->
      return (File.events file)

die :: Show a => a -> IO b
die s = do
  print s
  exitWith (ExitFailure 1)
