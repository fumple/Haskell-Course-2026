import System.Directory (removeDirectoryRecursive, createDirectory, setCurrentDirectory, getCurrentDirectory, doesDirectoryExist)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, after_, before_)
import Lib (Object(Blob), readObject, writeObject, calculateObjectHash)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import qualified Commands as C

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
        it "calculates the same hash for the same object" $ do
          let obj1 = Blob (BSL.pack "Hello World!")
          let obj2 = Blob (BSL.pack "Hello World!")
          let hash1 = calculateObjectHash obj1
          let hash2 = calculateObjectHash obj2
          hash1 `shouldBe` hash2
        it "returns Nothing when attempting to read an nonexisting object" $ do
          let hash = "66756d706c6566756d706c6566756d706c650000"
          result <- runMaybeT $ readObject hash
          result `shouldBe` (Nothing)

main :: IO ()
main = do
  s <- spec
  hspec s
