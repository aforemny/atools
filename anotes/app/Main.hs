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
    case includes of
      (i : is) ->
        fmap (map (\(r, fp) -> takeBaseName (S.toString r) ++ ":" ++ fp))
          . S.listFiles
          =<< S.open i is
      _ ->
        pure []

main :: IO ()
main =
  execParser mainArgs >>= \case
    ListArgs {..} -> do
      includes' <- splitOn ":" <$> getEnv "ATOOLS_PATH"
      case includes' ++ includes of
        (i : is) -> do
          mapM_ (\(r, fp) -> putStrLn (takeBaseName (S.toString r) ++ ":" ++ fp))
            =<< S.listFiles
            =<< S.open i is
    ShowArgs {..} -> do
      includes' <- splitOn ":" <$> getEnv "ATOOLS_PATH"
      case includes' ++ includes of
        (i : is) -> do
          h <- S.open i is
          putStrLn . maybe "" id =<< S.readFile h filePath
