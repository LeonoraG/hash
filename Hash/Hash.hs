-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.

module Hash.Hash where

import Hash.Language.Exec 
import Hash.Language.Expressions
import Hash.Parsing.HashParser
import Hash.Language.Commands

import Text.Parsec (parse, ParseError)

import qualified Data.Map as M


-- Reads a .hash script and runs it
runScript :: FilePath -> IO ()
runScript fp = do
    eitherLtlexpr <- tLExprsFromFile fp
    let ltlexpr = case eitherLtlexpr of
         Left err -> error "Script not formatted correctly"
         Right xs -> xs
    runHashProgram commands (Left ".") ltlexpr
    return ()
    
-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  continueParsing $ ScriptState { output = [], wd = [], vartable = M.empty}
  return ()
     
           
-- The program runs until we "quit"
continueParsing :: ScriptState -> IO ScriptState
continueParsing sstate = do
  newstate <- parseLine sstate
  if(wd newstate == ";quit") 
              then return newstate
              else continueParsing newstate
              
-- Enables us to perform commands line by line
parseLine :: ScriptState -> IO ScriptState
parseLine sstate = do
  cont <- getLine
  let parsed = parse parseTLExpr "Interactive" cont
  case parsed of
         Left err -> do
           putStrLn "Error: incorrect syntax"
           return sstate
         Right a  -> do
           newstate <- runHashProgram commands (Right sstate) [a] 
           return newstate