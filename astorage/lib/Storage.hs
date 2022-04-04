{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Storage
  ( Handle,
    Root,
    toString,
    deleteFile,
    fileExists,
    listFiles,
    open,
    readFile,
    writeFile,
  )
where

import Control.Exception
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Process
import Prelude hiding (readFile, writeFile)
import qualified Prelude

data Handle
  = Handle Root [Root]
  deriving (Show)

data Root
  = Root FilePath
  deriving (Show)

toString :: Root -> FilePath
toString (Root cwd) = cwd

open :: FilePath -> [FilePath] -> IO Handle
open fp fps = do
  Handle <$> (fromJust <$> root fp) <*> (catMaybes <$> mapM root fps)
  where
    root cwd = do
      cwd' <- canonicalizePath cwd
      cwd'' <- canonicalizePath =<< git cwd "rev-parse --show-toplevel"
      pure $
        if equalFilePath (cwd' ++ "\n") cwd''
          then Just (Root cwd')
          else Nothing

readFile :: Handle -> FilePath -> IO (Maybe String)
readFile (Handle r rs) fp = do
  first (r : rs) $ \(Root cwd) -> git cwd ("show HEAD:" ++ q fp)

fileExists :: Handle -> FilePath -> IO (Maybe Root)
fileExists (Handle r rs) fp =
  first (r : rs) $ \r@(Root cwd) -> const r <$> git cwd ("show HEAD:" ++ q fp)

writeFile :: Handle -> FilePath -> String -> IO Root
writeFile h@(Handle r@(Root cwd) _) fp s = do
  (r@(Root cwd), msg) <-
    fileExists h fp >>= \case
      (Just r) -> pure (r, "update " ++ q fp)
      Nothing -> pure (r, "init " ++ q fp)
  Prelude.writeFile (cwd </> fp) s
  git cwd ("add " ++ q fp)
  git cwd ("commit -m 'update " ++ q fp ++ "'")
  pure r

deleteFile :: Handle -> FilePath -> IO (Maybe Root)
deleteFile h fp =
  fileExists h fp >>= \case
    Just r@(Root cwd) -> do
      git cwd ("rm " ++ q fp)
      git cwd ("commit -m 'drop " ++ q fp ++ "'")
      pure (Just r)
    Nothing -> pure Nothing

listFiles :: Handle -> IO [(Root, FilePath)]
listFiles (Handle r rs) =
  concat <$> mapM (\r@(Root cwd) -> map (r,) . sort . lines <$> git cwd "ls-files") (r : rs)

first :: [a] -> (a -> IO b) -> IO (Maybe b)
first [] _ = pure Nothing
first (x : xs) f = (Just <$> f x) `catch` (\(e :: IOError) -> first xs f)

git :: FilePath -> String -> IO String
git cwd args =
  readCreateProcess ((shell ("git " ++ args ++ " 2>/dev/null")) {cwd = Just cwd}) ""

q :: String -> String
q s =
  show s
