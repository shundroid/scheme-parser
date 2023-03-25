module Main where

import Parser
import System.IO

main :: IO ()
main = do
  handle <- openFile "test/kadais28.scm" ReadMode
  contents <- hGetContents handle
  print $ parse contents
  hClose handle
