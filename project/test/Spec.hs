import System.Directory (removeDirectoryRecursive, createDirectory, setCurrentDirectory, getCurrentDirectory, doesDirectoryExist)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, after_, before_)
import Lib (Object(Blob), readObject, writeObject)
import qualified Data.ByteString.Lazy.Char8 as BSL

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
      describe "Unit Tests" $ do
        it "returns object correctly after writing" $ do
          let obj = Blob (BSL.pack "Hello World!")
          hash <- writeObject obj
          result <- readObject hash
          result `shouldBe` (Just obj)
        it "returns Nothing when attempting to read an nonexisting object" $ do
          let hash = "66756d706c6566756d706c6566756d706c650000"
          result <- readObject hash
          result `shouldBe` (Nothing)

main :: IO ()
main = do
  s <- spec
  hspec s
