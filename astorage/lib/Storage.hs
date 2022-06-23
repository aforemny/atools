{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Storage
  ( Handle,
    open,
    deleteFile,
    fileExists,
    listFiles,
    readFile,
    writeFile,
    Entry,
    fromString,
    toString
  )
where

import Control.Arrow
import Control.Exception
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.FilePath
import System.Process
import Prelude hiding (readFile, writeFile)
import qualified Prelude

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
        then fmap Just (f cwd) `catch` (\(_ :: IOError) -> pure Nothing)
        else go rs

fileExists :: Handle -> Entry -> IO Bool
fileExists h e@(Entry b (Rel fp)) = fmap isJust . withCwd h b $ \cwd ->
  git cwd ("show HEAD:" ++ q fp)

writeFile :: Handle -> Entry -> String -> IO Bool
writeFile h e@(Entry b (Rel fp)) s = do
  doesExist <- fmap isJust . withCwd h b $ \cwd -> git cwd ("show HEAD:" ++ q fp)
  fmap isJust . withCwd h b $ \cwd@(Abs cwd') -> do
    Prelude.writeFile (cwd' </> fp) s
    git cwd ("add " ++ q fp)
    let m = if doesExist then "update" else "init"
    git cwd ("commit -m '" ++ commitMessage m e ++ "'")

deleteFile :: Handle -> Entry -> IO ()
deleteFile h e@(Entry b (Rel fp)) = fmap (const ()) . withCwd h b $ \cwd -> do
  git cwd ("rm " ++ q fp)
  git cwd ("commit -m '" ++ commitMessage "delete" e ++ "'")

listFiles :: Handle -> IO [Entry]
listFiles =
  fmap concat
    . mapM
      ( \r@(Root cwd) ->
          map (Entry (base r) . Rel) . sort . lines <$> git cwd "ls-files"
      )
    . unHandle

git :: Abs FilePath -> String -> IO String
git (Abs cwd) args =
  readCreateProcess ((shell ("git " ++ args ++ " 2>/dev/null")) {cwd = Just cwd}) ""

commitMessage :: String -> Entry -> String
commitMessage m (Entry (Base b) (Rel fp)) =
  q b ++ "(" ++ q fp ++ "): " ++ q m

q :: String -> String
q s =
  show s
