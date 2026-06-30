module Main (main) where

import System.Environment (getArgs)
import qualified Commands as C

main :: IO ()
main = do
  args <- getArgs
  case args of
    "init" : _ -> C.runCommand (C.CInit)
    "add" : fp : _ -> C.runCommand (C.CAdd fp)
    "commit" : "-m" : msg : _ -> C.runCommand (C.CCommit msg)
    "log" : _ -> C.runCommand (C.CLog)
    "checkout" : cm : _ -> C.runCommand (C.CCheckout cm)
    _ -> do
      putStrLn "Available commands: init, add, commit, log, checkout"
