module Lib
( 
      writeObject,
      calculateObjectHash,
      writeIndex,
      writeBranch,
      writeHead,
      readObject,
      readIndex,
      readBranch,
      readHead,
      Object (..),
    ) where

import Types
import qualified Parser as P

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO (openFile, IOMode (ReadMode))
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.RWS (MonadTrans(lift))
import qualified Debug.Trace as DEBUG

-- Object Store
-- TODO: Look for the repo folder in the parent directory?
failIfMiniGitFolderDoesNotExist :: IO ()
failIfMiniGitFolderDoesNotExist = do
  exists <- doesDirectoryExist ".minigit/objects"
  if not exists
    then do
      putStrLn "This folder is not a repository!"
      exitWith (ExitFailure 1)
    else pure ()

--toByteString (Tree entries) = mconcat (BSB.stringUtf8 "tree\n" : map (\x -> (BSB.stringUtf8 $ entryName x)))
--  where
--    sorted = sortBy (\a b -> compare (entryName a) (entryName b)) entries

writeObject :: Object -> IO Hash 
writeObject obj = do
  -- Ensure correct directory exists
  failIfMiniGitFolderDoesNotExist

  -- Build final object contents
  let str = BSB.toLazyByteString $ P.serialize obj
  
  -- Calculate hash
  let hash = sha1 str 
  let hashstr = showDigest hash
  let prefix = take 2 hashstr

  -- Write object
  createDirectoryIfMissing False $ (".minigit/objects/" ++ prefix)
  BSL.writeFile (".minigit/objects/" ++ prefix ++ "/" ++ (drop 2 hashstr)) str

  -- Return hash
  return hashstr

calculateObjectHash :: Object -> Hash
calculateObjectHash obj = showDigest $ sha1 $ BSB.toLazyByteString $ P.serialize obj

writeIndex :: Hash -> IO ()
writeIndex hash = do
  -- Ensure correct directory exists
  failIfMiniGitFolderDoesNotExist

  writeFile ".minigit/INDEX" hash

writeHead :: Hash -> IO ()
writeHead hash = do
  -- Ensure correct directory exists
  failIfMiniGitFolderDoesNotExist

  writeFile ".minigit/HEAD" hash

writeBranch :: String -> Hash -> IO ()
writeBranch branch hash = do
  -- Ensure correct directory exists
  failIfMiniGitFolderDoesNotExist

  writeFile (".minigit/branches/"++branch) hash

readBranch :: String -> MaybeT IO Hash
readBranch branch = do
  let path = ".minigit/branches/" ++ branch
  exists <- lift $ doesFileExist path
  if (not exists)
  then fail "File not found!"
  else do
    handle <- lift $ openFile path ReadMode
    contents <- lift $ BS.hGetContents handle
    parseContents P.hash contents
  
readIndex :: MaybeT IO Hash
readIndex = do
  let path = ".minigit/INDEX"
  exists <- lift $ doesFileExist path
  if (not exists)
  then fail "File not found!"
  else do
    handle <- lift $ openFile path ReadMode
    contents <- lift $ BS.hGetContents handle
    parseContents P.hash contents

readHead :: MaybeT IO Hash
readHead = do
  let path = ".minigit/HEAD"
  exists <- lift $ doesFileExist path
  if (not exists)
  then fail "File not found!"
  else do
    handle <- lift $ openFile path ReadMode
    contents <- lift $ BS.hGetContents handle
    parseContents P.hash contents

readObject :: Hash -> MaybeT IO Object
readObject hash = do
  let prefix = take 2 hash
  let suffix = drop 2 hash
  let path = ".minigit/objects/" ++ prefix ++ "/" ++ suffix
  exists <- lift $ doesFileExist path
  if (not exists)
  then fail "File not found!"
  else do 
    handle <- lift $ openFile path ReadMode
    contents <- lift $ BS.hGetContents handle
    parseContents P.parser contents

parseContents :: P.Parser a -> BS.ByteString -> MaybeT IO a
parseContents p contents = do
  let parsed = P.runParser p contents
  case parsed of
    [] -> do
      DEBUG.traceStack "Failed to parse object!" $ pure ()
      lift $ putStrLn "Failed to parse object!"
      fail "Failed to parse object!"
    (v, str) : [] -> do
      if BS.empty == str
        then pure v
        else do
          DEBUG.traceStack "Failed to parse object! (Left-over str)" $ pure ()
          lift $ putStrLn "Failed to parse object! (Left-over str)"
          fail "Failed to parse object! (Left-over str)"
    _ -> do
      lift $ putStrLn "Failed to parse object! (multiple results found)"
      fail "Failed to parse object! (multiple results found)"

