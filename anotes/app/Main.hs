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
  = ListArgs {includes :: [FilePath]}
  | ShowArgs {includes :: [FilePath], filePath :: FilePath}

listArgs, showArgs :: Parser Args
listArgs = ListArgs <$> includesArg
showArgs = ShowArgs <$> includesArg <*> fileArg

listCmd, showCmd :: Mod CommandFields Args
listCmd = command "list" (info listArgs fullDesc)
showCmd = command "show" (info showArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (listCmd <> showCmd) <**> helper) idm

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
main =
  execParser mainArgs >>= \case
    ListArgs {..} -> do
      includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
      mapM_ (putStrLn . S.toString)
        =<< S.listFiles
        =<< S.open (includes' ++ includes)
    ShowArgs {..} -> do
      includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
      h <- S.open (includes' ++ includes)
      putStrLn . maybe "" id =<< S.readFile h (S.fromString filePath)
