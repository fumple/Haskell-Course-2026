module Main (main) where

import Lib
import qualified Data.ByteString.Lazy.Char8 as BSL

main :: IO ()
main = do
  let packedContents = BSL.pack "Hello World!"

  -- Write object
  hash <- writeObject (Blob packedContents)
  putStrLn hash

  -- Read back object
  obj <- readObject hash
  case obj of
    Just (Blob str) -> do 
      putStr "Blob: "
      BSL.putStrLn str
      if packedContents == str
        then putStrLn "Success! Contents match!"
        else putStrLn "Fail! Contents don't match!"
    Nothing -> putStrLn "Didn't find the object!"
    _ -> putStrLn "Got back unexpected object type!"
