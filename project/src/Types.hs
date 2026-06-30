module Types
  (
    Hash,
    Object (..),
    TreeEntry (..),
    EntryKind (..),
    CommitInfo (..),
  )
  where

import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString.Lazy as BSL

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

