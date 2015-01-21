
module Main where

import Hash.Hash
import Text.Parsec
import System.Environment

-- Reads command line arguments
-- runs Script if we have given a path, Interactive otherwise

main :: IO()
main = do
  args <- getArgs
  case args of
       []    -> runInteractive
       (h:t) -> runScript h t       
