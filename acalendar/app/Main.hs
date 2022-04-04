{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Options.Applicative (CommandFields, Completer, Mod, Parser, ParserInfo, action, bashCompleter, command, completer, execParser, fullDesc, help, helper, hsubparser, idm, info, metavar, strArgument, short, long, strOption)
import qualified System.Console.ANSI as Ansi
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((<.>), (</>))
import Text.ICalendar.Printer (printICalendar)
import Text.ICalendar.Types
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

exportArgs, listArgs :: Parser Args
exportArgs = ExportArgs <$> sourcesArg <*> outputArg
listArgs = ListArgs <$> sourcesArg

exportCmd, listCmd :: Mod CommandFields Args
exportCmd = command "export" (info exportArgs fullDesc)
listCmd = command "list" (info listArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (listCmd <> exportCmd) <**> helper) idm

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
                                  (1 :: Int, start, - end, Event.title e)
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
