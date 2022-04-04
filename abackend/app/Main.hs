{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Function
import Data.List
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.Directory
import System.FilePath

data Args = ServeArgs
  { root :: FilePath
  }

serveArgs :: Parser Args
serveArgs =
  ServeArgs <$> rootArg

serveCmd :: Mod CommandFields Args
serveCmd =
  command "serve" (info serveArgs fullDesc)
    <> metavar "serve"

mainArgs :: ParserInfo Args
mainArgs =
  info (hsubparser serveCmd <**> helper) idm

rootArg :: Parser FilePath
rootArg =
  strArgument
    ( metavar "DIR"
        <> help "Root directory"
    )

main :: IO ()
main = do
  args <- execParser mainArgs
  setCurrentDirectory (root args)
  putStrLn "http://localhost:8080"
  run 8080 app

app :: Application
app request respond = do
  case rawPathInfo request of
    filePath' -> do
      let filePath = "." ++ B8.unpack filePath'
      case requestMethod request of
        "GET" -> do
          doesFileExist filePath >>= \case
            True ->
              respond
                . responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                =<< LB8.readFile filePath
            False ->
              respond
                . responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                . (fromString . show)
                . sort
                . filter (not . ("~" `isSuffixOf`))
                =<< listDirectory (filePath)
        "HEAD" -> do
          doesFileExist filePath >>= \case
            True -> do
              respond $
                responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  ""
        "POST" -> do
          doesFileExist filePath >>= \case
            False -> do
              body <- do
                let loop a = do
                      cs <- getRequestBodyChunk request
                      ( if cs == B8.empty
                          then return
                          else loop
                        )
                        (a `B8.append` cs)
                loop ""
              _ <- LB8.writeFile filePath (LB8.fromStrict body)
              respond
                . responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                =<< LB8.readFile filePath
        "PUT" -> do
          doesFileExist filePath >>= \case
            True -> do
              body <- do
                let loop a = do
                      cs <- getRequestBodyChunk request
                      ( if cs == B8.empty
                          then return
                          else loop
                        )
                        (a `B8.append` cs)
                loop ""
              _ <- LB8.writeFile filePath (LB8.fromStrict body)
              respond
                . responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                =<< LB8.readFile filePath
