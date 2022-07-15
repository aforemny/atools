{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Storage
  ( Handle,
    open,
    deleteFile,
    editFile,
    fileExists,
    listFiles,
    readFile,
    writeFile,
    Entry,
    fromString,
    toString,
  )
where

import Control.Arrow
import Control.Exception
import qualified Control.Exception as C
import Control.Monad
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.IO.Exception (IOErrorType (OtherError))
import qualified GHC.IO.Handle.Types as GHC
import System.Directory
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Error (mkIOError)
import System.IO.Strict (SIO)
import qualified System.IO.Strict as SIO
import System.Process
import Prelude hiding (readFile, writeFile)

data Handle = Handle {unHandle :: [Root]}

data Root = Root (Abs FilePath)

data Base = Base String deriving (Eq)

base :: Root -> Base
base (Root (Abs cwd)) = Base (takeBaseName cwd)

data Entry = Entry Base (Rel FilePath)

fromString :: String -> Entry
fromString s =
  let (b : fp') = splitOn ":" s
   in Entry (Base b) (Rel (intercalate ":" fp'))

toString :: Entry -> String
toString (Entry (Base b) (Rel fp)) = b ++ ":" ++ fp

data Abs a
  = Abs a

data Rel a
  = Rel a

open :: [FilePath] -> IO Handle
open fps = Handle . rights <$> mapM root fps
  where
    root cwd = do
      cwd' <- canonicalizePath cwd
      cwd'' <- canonicalizePath =<< git (Abs cwd) "rev-parse --show-toplevel"
      pure $
        if equalFilePath (cwd' ++ "\n") cwd''
          then Right (Root (Abs cwd'))
          else Left cwd

readFile :: Handle -> Entry -> IO (Maybe String)
readFile h e@(Entry b (Rel fp)) = withCwd h b $ \cwd ->
  git cwd ("show HEAD:" ++ q fp)

withCwd :: Handle -> Base -> (Abs FilePath -> IO a) -> IO (Maybe a)
withCwd (Handle rs) b f = go rs
  where
    go [] = pure Nothing
    go (r@(Root cwd) : rs) =
      if base r == b
        then
          fmap Just (f cwd)
            `catch` ( \(e :: IOError) -> do
                        hPutStrLn stderr ("caught: " ++ show e)
                        pure Nothing
                    )
        else go rs

fileExists :: Handle -> Entry -> IO Bool
fileExists h e@(Entry b (Rel fp)) = fmap isJust . withCwd h b $ \cwd ->
  git cwd ("show HEAD:" ++ q fp)

writeFile :: Handle -> Entry -> String -> IO Bool
writeFile h e@(Entry b (Rel fp)) s = do
  doesExist <- fileExists h e
  fmap isJust . withCwd h b $ \cwd@(Abs cwd') -> do
    SIO.run $ SIO.writeFile (cwd' </> fp) s
    git cwd ("add " ++ q fp)
    let m = if doesExist then "update" else "init"
    git cwd ("commit -m " ++ q (commitMessage m e))

editFile :: Handle -> Entry -> IO ()
editFile h e@(Entry b (Rel fp)) = do
  doesExist <- fileExists h e
  fmap (const ()) . withCwd h b $ \cwd@(Abs cwd') -> do
    s <-
      if doesExist
        then SIO.run $ SIO.readFile (cwd' </> fp)
        else pure ""
    withCwd h b $ \cwd -> editor cwd (q fp)
    s' <- SIO.run $ SIO.readFile (cwd' </> fp)
    when (s /= s') $ do
      git cwd ("add " ++ q fp)
      let m = if doesExist then "update" else "init"
      git cwd ("commit -m " ++ q (commitMessage m e))
      pure ()

deleteFile :: Handle -> Entry -> IO ()
deleteFile h e@(Entry b (Rel fp)) = fmap (const ()) . withCwd h b $ \cwd -> do
  git cwd ("rm " ++ q fp)
  git cwd ("commit -m " ++ q (commitMessage "delete" e))

listFiles :: Handle -> IO [Entry]
listFiles =
  fmap concat
    . mapM
      ( \r@(Root cwd) ->
          map (Entry (base r) . Rel) . sort . lines <$> git cwd "ls-files"
      )
    . unHandle

git :: Abs FilePath -> String -> IO String
git (Abs cwd) args = do
  -- hPutStrLn stderr ("+ " ++ cmd)
  readCreateProcess ((shell cmd) {cwd = Just cwd}) ""
  where
    cmd = ("git " ++ args ++ " 2>/dev/null")

editor :: Abs FilePath -> String -> IO ()
editor (Abs cwd) args = do
  -- hPutStrLn stderr ("+ " ++ cmd)
  callCreateProcess ((shell cmd) {cwd = Just cwd})
  where
    cmd = ("${EDITOR-vi} " ++ args ++ " 2>/dev/null")

commitMessage :: String -> Entry -> String
commitMessage m (Entry (Base b) (Rel fp)) =
  case fmap (takeDirectory *** joinPath) (uncons (splitPath fp)) of
    Just (d, fp) -> d ++ "(" ++ fp ++ "): " ++ m
    Nothing -> fp ++ ": " ++ m

q :: String -> String
q s =
  show s

callCreateProcess :: CreateProcess -> IO ()
callCreateProcess cp = do
  exit_code <- withCreateProcess_
    "callCommand"
    cp {delegate_ctlc = True}
    $ \_ _ _ p ->
      waitForProcess p
  case exit_code of
    ExitSuccess -> return ()
    ExitFailure r -> processFailedException "callCommand" cmd args r
  where
    (cmd, args) =
      case cmdspec cp of
        ShellCommand cmd -> (cmd, [])
        RawCommand fp args -> (fp, args)

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
  ioError
    ( mkIOError
        OtherError
        ( fun ++ ": " ++ cmd
            ++ concatMap ((' ' :) . show) args
            ++ " (exit "
            ++ show exit_code
            ++ ")"
        )
        Nothing
        Nothing
    )

withCreateProcess_ ::
  String ->
  CreateProcess ->
  (Maybe GHC.Handle -> Maybe GHC.Handle -> Maybe GHC.Handle -> ProcessHandle -> IO a) ->
  IO a
withCreateProcess_ fun c action =
  C.bracketOnError
    (createProcess_ fun c)
    cleanupProcess
    (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
