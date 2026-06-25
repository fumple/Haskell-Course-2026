module Lib
    ( 
      writeObject,
      readObject,
      Object (..),
    ) where

import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO (openFile, IOMode (ReadMode))

-- Each object is identified by the hash of its contents.
type Hash = String

data Object
  = Blob   BSL.ByteString
  | Tree   [TreeEntry]
  | Commit CommitInfo
  deriving (Show, Eq)

data TreeEntry = TreeEntry
  { entryName :: FilePath
  , entryHash :: Hash
  , entryKind :: EntryKind        -- file vs. nested tree
  }
  deriving (Show, Eq)

data EntryKind = File | Dir-- | ...
  deriving (Show, Eq)

data CommitInfo = CommitInfo
  { commitTree    :: Hash
  , commitParent  :: Maybe Hash
  , commitAuthor  :: String
  , commitMessage :: String
  , commitTime    :: POSIXTime           -- POSIXTime, ZonedTime, your choice
  }
  deriving (Show, Eq)

-- Object Store
ensureObjectStoreExists :: IO ()
ensureObjectStoreExists = do
  createDirectoryIfMissing False ".minigit"
  createDirectoryIfMissing False ".minigit/objects"

toByteString :: Object -> BSB.Builder
toByteString (Blob str) = BSB.stringUtf8 "blob\n" <> BSB.lazyByteString str
toByteString _ = undefined

writeObject :: Object -> IO Hash 
writeObject obj = do
  -- Create object store paths
  ensureObjectStoreExists

  -- Build final object contents
  let str = BSB.toLazyByteString $ toByteString obj
  
  -- Calculate hash
  let hash = sha1 str 
  let hashstr = showDigest hash
  let prefix = take 2 hashstr

  -- Write object
  createDirectoryIfMissing False $ (".minigit/objects/" ++ prefix)
  BSL.writeFile (".minigit/objects/" ++ prefix ++ "/" ++ (drop 2 hashstr)) str

  -- Return hash
  return hashstr

readObject :: Hash -> IO (Maybe Object)
readObject hash = do
  let prefix = take 2 hash
  let suffix = drop 2 hash
  let path = ".minigit/objects/" ++ prefix ++ "/" ++ suffix
  exists <- doesFileExist path
  if (not exists)
  then return Nothing
  else do 
    handle <- openFile path ReadMode
    contents <- BS.hGetContents handle
    let (header_, content_) = BS.span (\c -> c /= '\n') contents
    let header = BS.unpack header_
    let content = BS.drop 1 content_
    return $ case header of
      "blob" -> Just (Blob (BSL.fromStrict content))
  
