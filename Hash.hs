-- The top-level module. 
-- Connects parsing to execution and adds interaction with the user 
-- or reading from file.

module Hash where

import Prelude hiding (catch) 
import Control.Exception
import Language.Exec 
import Language.Expressions
import Parsing.HashParser
import Language.Commands

import Text.Parsec (parse, ParseError)

import qualified Data.Map as M


-- Reads a .hash script and runs it
runScript :: FilePath -> [String] -> IO ()
runScript fp args = do
    ltlexpr <- catch (parseTLExprsFromFile fp args) $ catchParseExp
    catch (runHashProgram commands (Left ".") ltlexpr) $ catchErr (ScriptState {output="", wd="", vartable = M.empty}) "Invalid usage of functions"
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
           putStrLn "Error: invalid syntax"
           return sstate              
         Right a  -> do
           newstate <- catch (runHashProgram commands (Right sstate) [a] ) $ catchErr sstate "Invalid usage"
           return newstate
           
catchParseExp :: SomeException -> IO [TLExpr]
catchParseExp e = putStrLn "Invalid script syntax" >> return []  

catchErr :: ScriptState -> String -> SomeException -> IO ScriptState
catchErr sstate message _= putStrLn message >> return sstate
