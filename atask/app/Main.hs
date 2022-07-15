{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import Options.Applicative
import qualified Storage as S
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Unsafe
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as P

data Args = ListArgs {includes :: [FilePath]}

listArgs :: Parser Args
listArgs = ListArgs <$> includesArg

listCmd :: Mod CommandFields Args
listCmd = command "list" (info listArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser listCmd <**> helper) idm

includesArg :: Parser [FilePath]
includesArg = many (strOption (long "include" <> short 'I' <> metavar "DIR"))

main :: IO ()
main = do
  includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
  execParser mainArgs >>= \case
    ListArgs {..} ->
      do
        h <- S.open (includes' ++ includes)
        mapM_
          ( \f -> do
              Just c <- S.readFile h f
              P.Pandoc m p <- P.runIOorExplode (P.readMarkdown rOpts (T.pack c))
              putStrLn (S.toString f)
              T.putStrLn
                =<< P.runIOorExplode
                  ( P.writeMarkdown
                      wOpts
                      (P.Pandoc m (P.query taskListItem p))
                  )
          )
          . filter isMeeting
          =<< S.listFiles h

taskListItem b@(P.Para is@(P.Str "☐" : P.Space : _)) = [b]
-- taskListItem b@(P.Para is@(P.Str "☒" : P.Space : _)) = [b]
-- taskListItem b@(P.Para is@(P.Str "✓" : P.Space : _)) = [b]
taskListItem b@(P.Para is@(P.Str "❏" : P.Space : _)) = [b]
taskListItem b@(P.Para _) = []
taskListItem b@(P.Plain is@(P.Str "☐" : P.Space : _)) = [b]
-- taskListItem b@(P.Plain is@(P.Str "☒" : P.Space : _)) = [b]
-- taskListItem b@(P.Plain is@(P.Str "✓" : P.Space : _)) = [b]
taskListItem b@(P.Plain is@(P.Str "❏" : P.Space : _)) = [b]
taskListItem b@(P.Plain _) = []
taskListItem b = []

rOpts =
  P.def
    { P.readerExtensions = P.pandocExtensions,
      P.readerStandalone = True
    }

wOpts =
  P.def
    { P.writerExtensions = P.pandocExtensions,
      P.writerTemplate = Just writerTemplate
    }

writerTemplate =
  (unsafePerformIO (P.runIOorExplode (P.compileDefaultTemplate "markdown")))
{-# NOINLINE writerTemplate #-}

pExts =
  P.extensionsFromList
    [ P.Ext_yaml_metadata_block,
      P.Ext_backtick_code_blocks,
      P.Ext_fenced_code_attributes,
      P.Ext_intraword_underscores,
      P.Ext_blank_before_blockquote,
      P.Ext_blank_before_header,
      P.Ext_strikeout,
      P.Ext_task_lists,
      P.Ext_smart
    ]

isMeeting = (==) "meetings" . takeDirectory . S.filePath
