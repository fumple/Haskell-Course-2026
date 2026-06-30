module Commands (runCommand, Command (..))
where
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import Lib
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import System.FilePath (splitDirectories)
import Control.Monad.RWS (MonadTrans(lift))
import qualified Data.ByteString.Lazy as BSL
import Types
import Data.Time.Clock.POSIX (getPOSIXTime)
import Debug.Trace (trace)

data Command = CInit | CAdd FilePath | CCommit String | CLog | CCheckout String
runCommand :: Command -> IO ()
runCommand (CInit) = commandInit
runCommand (CAdd fp) = maybeCommandWrapper (commandAdd fp)
runCommand (CCommit msg) = maybeCommandWrapper (commandCommit msg)
runCommand (CLog) = maybeCommandWrapper (commandLog)
runCommand (CCheckout commit) = maybeCommandWrapper (commandCheckout commit)

maybeCommandWrapper :: MaybeT IO () -> IO ()
maybeCommandWrapper mt = do
  result <- runMaybeT mt
  case result of
    Just _ -> pure ()
    Nothing -> do
      -- TODO: Improve error handling
      putStrLn "Failed to execute command!"
      fail "Failed to execute command!"

commandInit :: IO ()
commandInit = do
  exists <- doesDirectoryExist ".minigit"
  if exists
    then do
      putStrLn ".minigit directory already exists!"
    else do
      createDirectoryIfMissing False ".minigit"
      createDirectoryIfMissing False ".minigit/objects"
      createDirectoryIfMissing False ".minigit/branches"
      let tree = Tree []
      hash <- writeObject tree
      _ <- writeIndex hash
      pure ()

commandAdd :: FilePath -> MaybeT IO ()
commandAdd fp = do
  --fullDir <- lift $ makeAbsolute fp
  --let parts = tail $ splitDirectories fullDir -- the first part is "/"
  let parts = splitDirectories fp
  --lift $ putStrLn $ "Parts: " ++ (show parts)
  indexHash <- readIndex
  index <- readObject indexHash
  contents <- lift $ BSL.readFile fp
  objectHash <- lift $ writeObject (Blob contents)
  newIndex <- add index objectHash parts
  lift $ writeIndex newIndex
  pure ()
  where
    add :: Object -> String -> [String] -> MaybeT IO String
    add (Tree entries) h (x:xs) = do 
      newEntries <- add_ entries h x xs
      let newTree = (Tree newEntries) 
      lift $ writeObject newTree
    add _ _ _ = do
      lift $ putStrLn "Expected index to point to tree, but it pointed to something else!"
      fail "index broken"
      
    add_ :: [TreeEntry] -> String -> String -> [String] -> MaybeT IO [TreeEntry] 
    add_ [] h x [] = do
      let entry = TreeEntry {entryName=x, entryKind=File, entryHash=h}
      return [entry]

    add_ [] h x xs = do
      subtree <- add (Tree []) h xs
      let entry = TreeEntry {entryName=x, entryKind=Dir, entryHash=subtree}
      return [entry]
      
    add_ (e:es) h x [] = do
      if (entryName e) == x
        then do
          let entry = TreeEntry {entryName=x, entryKind=File, entryHash=h}
          return $ entry : es
        else do
          rest <- add_ es h x []
          return $ e : rest

    add_ (e:es) h x xs = do
      if (entryName e) == x
        then do
          subtree <- add (Tree []) h xs
          let entry = TreeEntry {entryName=x, entryKind=Dir, entryHash=subtree}
          return $ entry : es
        else do
          rest <- add_ es h x xs
          return $ e : rest

commandCommit :: String -> MaybeT IO ()
commandCommit msg = do
  headHash <- lift $ runMaybeT readHead
  indexHash <- readIndex
  time <- lift $ getPOSIXTime
  let ci = CommitInfo {
    commitTree=indexHash,
    commitTime=time,
    commitParent=headHash,
    commitMessage=msg,
    commitAuthor="Placeholder <placeholder@example.com>"
  }
  ch <- lift $ writeObject (Commit ci)
  let prefix = take 6 ch
  _ <- lift $ putStrLn $ "[main " ++ prefix ++ "] " ++ msg
  lift $ writeBranch "main" ch
  lift $ writeHead ch
  pure ()

commandLog :: MaybeT IO ()
commandLog = do
  headHash <- lift $ runMaybeT readHead
  case headHash of
    Nothing -> do
      _ <- lift $ putStrLn "No commits found!"
      pure ()
    Just hh -> do
      printCommit hh
  where
    printCommit :: Hash -> MaybeT IO ()
    printCommit ch = do
      commit <- readObject ch
      case commit of
        Commit ci -> do
          --let prefix = take 6 ch
          _ <- lift $ putStrLn (ch ++ " " ++ (commitMessage ci))
          case (commitParent ci) of
            Just parent -> printCommit parent
            Nothing -> pure ()
        _ -> do
          _ <- lift $ putStrLn "Expected a commit, found something else!"
          fail ""

commandCheckout :: String -> MaybeT IO ()
commandCheckout ch = do
  headHash <- readHead
  headCommit <- readObject headHash
  commit <- readObject ch
  case (headCommit, commit) of
    (Commit hci, Commit ci) -> do
      tree1 <- readObject (commitTree hci)
      tree2 <- readObject (commitTree ci) 
      case (tree1, tree2) of
        (Tree oldEntries, Tree newEntries) -> do
          _ <- ensureThatChangesWontBeLost oldEntries newEntries "."
          lift $ putStrLn "No changes will be lost!"
        _ -> do
          lift $ putStrLn "Expected two trees to be attached, found something else!"
          fail ""
    _ -> do
      lift $ putStrLn $ "Expected two commits, found something else!"
      fail ""
  where
    -- Trusting, that the on disk files (inputs) were sorted
    ensureThatChangesWontBeLost :: [TreeEntry] -> [TreeEntry] -> String -> MaybeT IO ()
    ensureThatChangesWontBeLost [] y path = do
      trace ("ensureThatChangesWontBeLost called with:\n[]\n" ++
        show y ++ "\n" ++ show path) $ pure ()
      pure ()
    ensureThatChangesWontBeLost (x:xs) [] path = do
      let xn = entryName x
      let xk = entryKind x
      let xh = entryHash x
      let fullpath = path ++ "/" ++ xn
      case xk of
        Dir -> do
          subtree <- readObject xh
          case subtree of
            Tree subentries -> do
              ensureThatChangesWontBeLost subentries [] fullpath
              ensureThatChangesWontBeLost xs [] path
            _ -> do
              _ <- lift $ putStrLn "Encountered unexpected object!"
              fail ""
        File -> do
          currentFile <- lift $ BSL.readFile (path ++ "/" ++ xn) 
          let currentFileHash = calculateObjectHash (Blob currentFile)
          if currentFileHash /= xh
            then do
              _ <- lift $ putStrLn $ (path ++ "/" ++ xn) ++ " was modified!"
              fail ""
            else do
              ensureThatChangesWontBeLost xs [] path
    ensureThatChangesWontBeLost (x:xs) (y:ys) path = do
      let xn = entryName x
      let yn = entryName y
      let xk = entryKind x
      let yk = entryKind y
      let xh = entryHash x
      let yh = entryHash y
      {-if xn == yn && xh == yh
        then pure ()
      else-}
      if xn == yn && xk == File
        then do
          currentFile <- lift $ BSL.readFile (path ++ "/" ++ xn) 
          let currentFileHash = calculateObjectHash (Blob currentFile)
          if currentFileHash /= xh
            then do
              _ <- lift $ putStrLn $ (path ++ "/" ++ xn) ++ " was modified!"
              fail ""
            else do
              ensureThatChangesWontBeLost xs ys path
      else if xn == yn && xk == yk && xk == Dir
        then do
          tree1 <- readObject xh
          tree2 <- readObject yh
          case (tree1, tree2) of
            (Tree t1, Tree t2) -> do
              ensureThatChangesWontBeLost t1 t2 (path ++ "/" ++ xn)
              ensureThatChangesWontBeLost xs ys path
            _ -> do
              _ <- lift $ putStrLn "Invalid data found!"
              fail ""
      else if xn == yn && xk == Dir -- xk /= yk, so yk == File
        then do
          tree <- readObject xh
          case tree of
            Tree t -> do
              ensureThatChangesWontBeLost t [] (path ++ "/" ++ xn)
              ensureThatChangesWontBeLost xs ys path
            _ -> do
              _ <- lift $ putStrLn "Invalid data found!"
              fail ""
      else if xn > yn -- so there is a new file in Y that wasn't in x
        then do
          exists <- lift $ doesFileExist (path ++ "/" ++ yn)
          if exists
            then do
              currentFile <- lift $ BSL.readFile (path ++ "/" ++ yn) 
              let currentFileHash = calculateObjectHash (Blob currentFile)
              if currentFileHash /= yh
                then do
                  _ <- lift $ putStrLn $ (path ++ "/" ++ yn) ++ " would be overwritten!"
                  fail ""
                else pure ()
            else pure ()
          ensureThatChangesWontBeLost (x:xs) ys path
      else if xn < yn -- so there is a file in X that is no longer in Y
        then do
          currentFile <- lift $ BSL.readFile (path ++ "/" ++ xn) 
          let currentFileHash = calculateObjectHash (Blob currentFile)
          if currentFileHash /= xh
            then do
              _ <- lift $ putStrLn $ (path ++ "/" ++ xn) ++ " was modified when it would be removed!"
              fail ""
            else do
              ensureThatChangesWontBeLost xs (y:ys) path
      else do
        -- this should be every case
        _ <- lift $ putStrLn $ "this should be unreachable"
        fail ""

