module Parser (
  Serializable (..),
  Parser,
  runParser,
  hash,
  time
)
where

import Control.Monad.State
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit, ord)
import Data.List (sortOn)
import Types
import Data.Time.Clock.POSIX (POSIXTime)
import Text.Read (readMaybe)

-- -----
-- Parser code, from the "Calculator" example
-- -----

type Parser a = StateT BS.ByteString [] a

runParser :: Parser a -> BS.ByteString -> [(a, BS.ByteString)]
runParser = runStateT

zero :: Parser a
zero = StateT (const [])

item :: Parser Char
item = do
  s <- get
  let ht = BS.uncons s
  case ht of
    Just (h,t) -> put t >> pure h
    Nothing -> zero

remainingItems :: Parser BS.ByteString
remainingItems = do
  s <- get
  put BS.empty
  pure s

infixr 5 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s ->
  case runStateT p1 s of
    [] -> runStateT p2 s
    parses -> parses


sat :: (Char -> Bool) -> Parser Char
sat predicate = do
  c <- item
  if predicate c then pure c else zero

char :: Char -> Parser Char
char c = sat (== c)

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  pure (x : xs)

string :: String -> Parser String
string [] = pure []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  pure (c : cs)

-- TODO: Also check length?
hash :: Parser Hash
hash = many $ sat (\c -> isDigit c ||
  (fromIntegral (ord c - ord 'a')::Word) <= 5)

hashMaybe :: Parser (Maybe Hash)
hashMaybe = none <|> hash_
  where
    none :: Parser (Maybe Hash)
    none = do
      _ <- string "none"
      pure Nothing
    hash_ :: Parser (Maybe Hash)
    hash_ = do
      h <- hash
      pure $ Just h

nullTerminatedString :: Parser String
nullTerminatedString = many $ sat (\c -> c /= '\0')

time :: Parser POSIXTime
time = do
  word <- many $ sat (\c -> isDigit c || c == '.' || c == 's')
  case readMaybe word of
    Just t -> pure t
    Nothing -> zero
  

class Serializable a where
  serialize :: a -> BSB.Builder
  parser :: Parser a

-- not allowed! the other solution would be to build an instance like:
--   instance Serializable a => Serializable [a] where
-- but this could have bad consequences
--
--instance Serializable String where
--  serialize str = BSB.stringUtf8 str

instance Serializable EntryKind where
  serialize File = BSB.stringUtf8 "file"
  serialize Dir = BSB.stringUtf8 "dir"
  parser = file <|> dir
    where
      file = do
        _ <- string "file"
        pure File
      dir = do
        _ <- string "dir"
        pure Dir

instance Serializable TreeEntry where
  serialize entry = BSB.stringUtf8 ((entryHash entry) ++ " ") <>
    serialize (entryKind entry) <>
    BSB.stringUtf8 " " <>
    BSB.stringUtf8 (entryName entry) <>
    BSB.stringUtf8 "\0\n"
  parser = do
    h <- hash
    _ <- char ' '
    k <- parser
    _ <- char ' '
    fn <- nullTerminatedString
    _ <- string "\0\n"
    pure (TreeEntry {entryName=fn, entryHash=h, entryKind=k})

instance Serializable CommitInfo where
  serialize ci = BSB.stringUtf8 ("tree " ++ (commitTree ci) ++ "\n") <>
    BSB.stringUtf8 ("parent " ++ (parent $ commitParent ci) ++ "\n") <>
    BSB.stringUtf8 ("author " ++ (commitAuthor ci) ++ "\0\n") <>
    BSB.stringUtf8 ("message " ++ (commitMessage ci) ++ "\0\n") <>
    BSB.stringUtf8 ("time " ++ (show $ commitTime ci) ++ "\n")
    where
      parent :: Maybe Hash -> String
      parent (Just h) = h
      parent Nothing = "none"
  parser = do
    _ <- string "tree "
    treeHash <- hash
    _ <- string "\nparent "
    parentHash <- hashMaybe
    _ <- string "\nauthor "
    author <- nullTerminatedString
    _ <- string "\0\nmessage "
    message <- nullTerminatedString
    _ <- string "\0\ntime "
    ctime <- time 
    _ <- char '\n'
    pure (CommitInfo {
      commitTree=treeHash,
      commitTime=ctime,
      commitParent=parentHash,
      commitMessage=message,
      commitAuthor=author
    })
    

instance Serializable Object where
  serialize (Blob str) = BSB.stringUtf8 "blob\n" <> BSB.lazyByteString str
  serialize (Tree entries) = BSB.stringUtf8 "tree\n" <> foldMap serialize sorted
    where
      sorted = sortOn (\e -> entryName e) entries
  serialize (Commit ci) = BSB.stringUtf8 "commit\n" <> serialize ci
  parser = blob <|> tree <|> commit
    where
      blob = do
        _ <- string "blob\n"
        str <- remainingItems
        pure (Blob $ BSL.fromStrict str)
      tree = do
        _ <- string "tree\n" 
        entries <- many parser
        pure (Tree entries)
      commit = do
        _ <- string "commit\n"
        ci <- parser
        pure (Commit ci)

