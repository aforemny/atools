{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List
import Data.List.Split
import Debug.Trace
import Options.Applicative
import qualified Storage as S
import System.Environment
import System.Exit
import System.FilePath

data Args
  = EditArgs {includes :: [FilePath], filePath :: FilePath}
  | ListArgs {includes :: [FilePath]}
  | ShowArgs {includes :: [FilePath], filePath :: FilePath}

editArgs, listArgs, showArgs :: Parser Args
editArgs = EditArgs <$> includesArg <*> fileArg
listArgs = ListArgs <$> includesArg
showArgs = ShowArgs <$> includesArg <*> fileArg

editCmd, listCmd, showCmd :: Mod CommandFields Args
editCmd = command "edit" (info editArgs fullDesc)
listCmd = command "list" (info listArgs fullDesc)
showCmd = command "show" (info showArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (editCmd <> listCmd <> showCmd) <**> helper) idm

includesArg :: Parser [FilePath]
includesArg = many (strOption (long "include" <> short 'I' <> metavar "DIR"))

fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE" <> completer fileArgComp)

fileArgComp :: Completer
fileArgComp =
  mkCompleter $ \input -> do
    includes <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
    fmap (map S.toString) . S.listFiles =<< S.open includes

main :: IO ()
main = do
  includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
  execParser mainArgs >>= \case
    EditArgs {..} -> do
      h <- S.open (includes' ++ includes)
      S.editFile h (S.fromString filePath)
    ListArgs {..} -> do
      mapM_ (putStrLn . S.toString)
        =<< S.listFiles
        =<< S.open (includes' ++ includes)
    ShowArgs {..} -> do
      h <- S.open (includes' ++ includes)
      putStrLn . maybe "" id =<< S.readFile h (S.fromString filePath)
