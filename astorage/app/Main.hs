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
      case includes of
        (i : is) -> do
          h <- open i is
          deleteFile h filePath
          pure ()
    ListArgs {..} -> do
      case includes of
        (i : is) -> do
          h <- open i is
          mapM_ (\(r, fp) -> putStrLn (toString r ++ ":" ++ fp))
            =<< listFiles h
    ReadArgs {..} -> do
      case includes of
        (i : is) -> do
          h <- open i is
          maybe (exitWith (ExitFailure 1)) putStrLn =<< readFile h filePath
          pure ()
    WriteArgs {..} -> do
      case includes of
        (i : is) -> do
          h <- open i is
          writeFile h filePath =<< getContents
          pure ()
  exitWith ExitSuccess
