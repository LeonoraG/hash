
module Main where

import Hash
import Text.Parsec
import System.Environment

-- Reads command line arguments
-- Runs Script if we have given a path, Interactive otherwise

main :: IO()
main = do
  args <- getArgs
  case args of
       []    -> runInteractive
       (h:t) -> runScript h t       
