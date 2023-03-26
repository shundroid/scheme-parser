module Main where

import Parser
import Runner
import System.IO
import Control.Monad.State
import Env
import Data.IORef

scheme = let topEnv = makeTopEnv in
  runStateT repLoop topEnv where
    repLoop :: StateT Env IO ()
    repLoop = do
      lift $ putStr "my-scheme> "
      input <- lift getLine
      env <- get
      exit <- lift $ newIORef False
      case parse input of
        Left memos -> lift $ putStrLn $ "ParseError: " ++ show memos
        Right node -> case eval (head node) env of
          Right (Exit, _) -> lift $ writeIORef exit True
          Right (node, env) -> do
            lift $ print node
            put env
          Left error -> lift $ putStrLn $ "RuntimeError: " ++ error
      doExit <- lift $ readIORef exit
      unless doExit repLoop

main :: IO ()
main = do
  handle <- openFile "test/kadais28.scm" ReadMode
  contents <- hGetContents handle
  print $ parse contents
  hClose handle
