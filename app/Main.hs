module Main where

import Parser
import Runner
import Control.Monad.State
import Env
import System.Exit (exitSuccess)
import Debug.Trace (trace)
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text

handleParse :: Text.Text -> StateT Env IO ()
handleParse text = do
  env <- get
  case parse text of
    Left memos -> lift $ putStrLn $ "ParseError: " ++ show memos
    Right nodes -> case eval (head nodes) env of
      Right (Command command, _) -> case command of
        Label "exit" -> lift exitSuccess
        Parens [Label "load", String fileName] -> do
          contents <- lift $ TextIO.readFile fileName
          handleParse contents
        _ -> lift $ putStrLn $ "Unknown command to interpreter: " ++ show command
      Right (node, env) -> do
        lift $ print node
        put env
      Left error -> lift $ putStrLn $ "RuntimeError: " ++ error

scheme = let topEnv = makeTopEnv in
  runStateT repLoop topEnv where
    repLoop :: StateT Env IO ()
    repLoop = do
      lift $ putStr "my-scheme> "
      input <- lift getLine
      handleParse $ Text.pack input
      repLoop

main = scheme
