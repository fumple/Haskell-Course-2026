import System.Directory (removeDirectoryRecursive, createDirectory, setCurrentDirectory, getCurrentDirectory, doesDirectoryExist)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, after_, before_, shouldNotBe)
import Lib (Object(Blob, Tree), readObject, writeObject, calculateObjectHash, readIndex, readHead)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import qualified Commands as C
import Types (TreeEntry(TreeEntry), EntryKind (File, Dir))

removeTestRunDirectory :: IO ()
removeTestRunDirectory = do
  dirExists <- doesDirectoryExist "./testrun"
  if (dirExists)
    then removeDirectoryRecursive "./testrun"
    else return ()

beforeHook :: Spec -> Spec
beforeHook = before_ $ do
  removeTestRunDirectory
  createDirectory "./testrun"
  setCurrentDirectory "./testrun"

afterHook :: FilePath -> Spec -> Spec
afterHook ogDir = after_ $ do
  setCurrentDirectory ogDir

spec :: IO Spec
spec = do
  originalDir <- getCurrentDirectory
  return $ beforeHook $ do
    afterHook originalDir $ do
      describe "Object Store - Unit Tests" $ do
        it "returns object correctly after writing" $ do
          C.runCommand (C.CInit)
          let obj = Blob (BSL.pack "Hello World!")
          hash <- writeObject obj
          result <- runMaybeT $ readObject hash
          result `shouldBe` (Just obj)
        it "calculates the same hash for the same object, and a different hash for a different object" $ do
          let obj1 = Blob (BSL.pack "Hello World!")
          let obj2 = Blob (BSL.pack "Hello World!")
          let obj3 = Blob (BSL.pack "Hello World1")
          let hash1 = calculateObjectHash obj1
          let hash2 = calculateObjectHash obj2
          let hash3 = calculateObjectHash obj3
          hash1 `shouldBe` hash2
          hash2 `shouldNotBe` hash3
        it "returns Nothing when attempting to read an nonexisting object" $ do
          let hash = "66756d706c6566756d706c6566756d706c650000"
          result <- runMaybeT $ readObject hash
          result `shouldBe` (Nothing)
        it "uses a consistent order for tree entries" $ do
          C.runCommand (C.CInit)
          let te1 = TreeEntry "a.txt" "1234" File
          let te2 = TreeEntry "b.txt" "1234" Dir
          let te3 = TreeEntry "c.txt" "1234" Dir
          let te4 = TreeEntry "d.txt" "1234" File
          let t1 = Tree [te1, te2, te3, te4]
          let t2 = Tree [te4, te3, te2, te1]
          h1 <- writeObject t1
          h2 <- writeObject t2
          obj1 <- runMaybeT $ readObject h1
          obj2 <- runMaybeT $ readObject h2
          h1 `shouldBe` h2
          obj1 `shouldBe` Just t1
          obj1 `shouldBe` obj2
      describe "End-to-end tests" $ do
        it "handles checkout correctly" $ do
          directoryShouldExist ".minigit" False

          C.runCommand (C.CInit)

          directoryShouldExist ".minigit" True
          directoryShouldExist ".minigit/objects" True
          directoryShouldExist ".minigit/branches" True

          writeFile "main.rs" "fn main() {}"

          C.runCommand (C.CAdd "main.rs")

          C.runCommand (C.CCommit "first commit")
          c1hash <- readHead_

          writeFile "main.rs" "fn main() { println!(\"Hello World!\") }"

          C.runCommand (C.CAdd "main.rs")

          C.runCommand (C.CCommit "second commit")

          C.runCommand (C.CCheckout c1hash) 

          mainContents <- readFile "main.rs"
          mainContents `shouldBe` "fn main() {}"

          chash <- readHead_
          chash `shouldBe` c1hash

directoryShouldExist :: FilePath -> Bool -> IO ()
directoryShouldExist dir expected = do
  exists <- doesDirectoryExist dir
  exists `shouldBe` expected

readHead_ :: IO String
readHead_ = do
  h <- runMaybeT readHead
  case h of
    Just hh -> pure hh
    Nothing -> fail ""

main :: IO ()
main = do
  s <- spec
  hspec s
