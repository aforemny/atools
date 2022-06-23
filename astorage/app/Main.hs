{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import Storage
import System.Exit
import Prelude hiding (readFile, writeFile)

data Args
  = DeleteArgs {includes :: [FilePath], filePath :: FilePath}
  | ListArgs {includes :: [FilePath]}
  | ReadArgs {includes :: [FilePath], filePath :: FilePath}
  | WriteArgs {includes :: [FilePath], filePath :: FilePath}

deleteArgs, listArgs, readArgs, writeArgs :: Parser Args
deleteArgs = DeleteArgs <$> includesArg <*> fileArg
listArgs = ListArgs <$> includesArg
readArgs = ReadArgs <$> includesArg <*> fileArg
writeArgs = WriteArgs <$> includesArg <*> fileArg

deleteCmd, listCmd, readCmd, writeCmd :: Mod CommandFields Args
deleteCmd = command "delete" (info deleteArgs fullDesc)
listCmd = command "list" (info listArgs fullDesc)
readCmd = command "read" (info readArgs fullDesc)
writeCmd = command "write" (info writeArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (deleteCmd <> listCmd <> readCmd <> writeCmd) <**> helper) idm

includesArg :: Parser [FilePath]
includesArg = some (strOption (long "include" <> short 'I' <> metavar "DIR"))

fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE")

main :: IO ()
main = do
  execParser mainArgs >>= \case
    DeleteArgs {..} -> do
      h <- open includes
      deleteFile h (fromString filePath)
      pure ()
    ListArgs {..} -> do
      mapM_ (putStrLn . toString) =<< listFiles =<< open includes
    ReadArgs {..} -> do
      h <- open includes
      maybe (exitWith (ExitFailure 1)) putStrLn =<< readFile h (fromString filePath)
      pure ()
    WriteArgs {..} -> do
      h <- open includes
      writeFile h (fromString filePath) =<< getContents
      pure ()
  exitWith ExitSuccess
