-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
module Language.Commands where

import qualified Data.Map as M
import Data.List
import Language.Expressions
import Parsing.HashParser
import Language.Exec 
import System.IO
import System.Directory
import Text.Parsec.String
import Text.ParserCombinators.Parsec

import Hexdump
import qualified Data.ByteString.Char8 as BS


commands :: M.Map String Command
commands =  M.fromList [ ("mv",mv), ("cp",cp), ("create", create) ,("rm",rm)
                       , ("cpdir", cpdir), ("mkdir",mkdir), ("rmdir", rmdir) 
                       , ("ls", ls), ("pwd", pwd), ("cd",cd), ("echo", echo)
                       , ("cat", cat), ("cmnt", comment), ("hexdump", hexd), ("quit", quit) ]
--  ("hexdump", hexd),


-- Implementations for these commands, in order of appearance :

-- Moves one or more files/directories to given target, recursively

mv  :: Command
mv []  _ = error "mv: No arguments given"
mv [_] _ = error "mv: Too few arguments"

mv [src,target] sstate = do
  sFileExists <- doesFileExist src
  sDirExists  <- doesDirectoryExist src
  tFileExists <- doesFileExist target
  tDirExists  <- doesDirectoryExist target
  case sFileExists of
       True -> case tDirExists of
                    False -> mvFileToFile [src,target] sstate
                    True -> mvFileToDirectory [src, target] sstate
       False-> case sDirExists of
                    True ->  renameDirectory src target >> return sstate
                    False -> error "mv: File doesn't exist!"
                    
                    
mv srcs sstate = do
    tDirExists <- doesDirectoryExist (last srcs)
    let f = case tDirExists of
         True  -> mvDirectoryToDirectory
         False -> mv
    let indivList = zipWith (\a b -> [a,b]) (init srcs) (repeat $ last srcs)
    mapM (flip f sstate) indivList 
    return sstate
    
mvFileToFile :: Command
mvFileToFile [src, target] sstate = do
    copyFile src target
    removeFile src
    return $ sstate { output=""}
    
mvFileToDirectory :: Command
mvFileToDirectory [src, target] sstate = do
    let targetF = getFolderName src target
    copyFile src targetF
    removeFile src
    return $ sstate { output = ""}
    
mvDirectoryToDirectory :: Command
mvDirectoryToDirectory [src, targetDir] sstate = do
    dirContents <- getDirectoryContents src
    createDirectory $ targetDir ++ "/" ++ src
    let contentsReal = map ((src++"/")++) $ delete ".." $ delete "." dirContents
    putStrLn $ show contentsReal;
    let indivList = zipWith (\a b -> [a,b]) contentsReal (repeat $ targetDir ++ "/"++src)
    mapM (flip mv sstate) indivList
    removeDirectoryRecursive src
    return $ sstate { output = ""}

-- Copies one or more files/directories to given target    
  
cp :: Command
cp [] _  = error "cp: No arguments given to cp"
cp [_] _ = error "cp: Too few arguments"
cp [src,target] sstate = do
    copyFile src target
    return $ sstate { output = ""}
    
cp srcs sstate = do
    let target = last srcs
    let indivList = zipWith (\a b -> [a,getFolderName a b]) (init srcs) (repeat $ target)
    mapM (flip cp sstate) indivList
    return $ sstate { output = ""}
    
-- Removes files/directories 

rm :: Command
rm [] _ = error "rm: No arguments given to rm"
rm srcs sstate = do
    mapM removeFile srcs
    return $ sstate { output = ""}
    
--Creates one or more empty files
create :: Command
create [] _ = error "create: No arguments given to create"
create [target] sstate = do
    existsF <- doesFileExist target 
    existsD <- doesDirectoryExist target
    let exists = existsF || existsD
    if exists then return $ sstate { output = ""}
              else do
                 openFile target WriteMode >>= hClose
                 return $ sstate { output = ""}
                 
cpdir :: Command
cpdir [] _ = error "cpdir: No arguments given"
cpdir [_] _= error "cpdir: No arguments given"
cpdir list sstate = do
    let indivList = zipWith (\a b -> [a,b] ) (init list) (repeat $ last list)
    mapM_ (flip cpDirectoryIntoDirectory sstate) indivList
    return $ sstate { output = ""}
    
cpDirectoryIntoDirectory :: Command
cpDirectoryIntoDirectory [src, targetDir] sstate = do
    contents <- getDirectoryContents src
    createDirectory $ targetDir ++ "/" ++ src
    let contentsReal = map ((src++"/")++) $ delete ".." $ delete "." contents
    putStrLn $ show contentsReal;
    let indivList = zipWith (\a b -> [a,b]) contentsReal (repeat $ targetDir ++ "/"++src)
    mapM_ (flip mv sstate) indivList
    return $ sstate { output=""}

mkdir :: Command
mkdir [] _ = error "mkdir: No arguments given"
mkdir [target] sstate = createDirectory target >> (return $ sstate { output=""})
mkdir list sstate = mapM_(\x -> mkdir [x] sstate) list >> (return $ sstate { output=""})

rmdir :: Command
rmdir [] _ = error "rmdir: No arguments given"
rmdir [target] sstate = do
    con <- getDirectoryContents target
    let content =  delete ".." $ delete "." con 
    case content of
         [] -> removeDirectory target >> (return $ sstate { output=""})
         _  -> error "Directory isn't empty"

-- Prints the contents of working directory

ls :: Command
ls [] sstate = ls ["."] sstate
ls list sstate = do
    con <- getDirectoryContents (head list)
    return $ sstate { output = unlines$con }
    
-- Prints the working directory

pwd :: Command
pwd [] sstate = do
    con <- getCurrentDirectory
    return $ sstate { output = con ++ "\n"}
    
pwd _ _ = error "pwd: Too many arguments"

-- Changes current working directory

cd :: Command
cd [] _ = error "cd: No arguments given"
cd [target] sset = do
    cur <- getCurrentDirectory
    setCurrentDirectory target
    return $ sset { wd = cur, output = "" }
  
cd _ _ = error "cd: Too many arguments given"


-- Prints to the screen, replacing variables with their values

echo :: Command
echo [target]  sstate = do
    let vals  = parse (many $ try varExp <|> readStrAnything ) "echo" target
    let vals2 = case vals of
                Left err -> error "echo: The parse failed"
                Right a  -> a
    let out = concat $ map (flip evalExpr (vartable sstate) ) vals2
    return $ sstate { output = out ++ "\n" }
    
-- Prints the content of file(s) to the screen
    
cat :: Command
cat [] sstate = error "cat: No arguments given"
cat list sstate = do
    ress <- mapM cath list
    return $ sstate { output = unlines $ ress }
    
-- Reads from sourcefile

cath :: FilePath -> IO String
cath src = readFile src

-- Hexdump

hexd :: Command
hexd list sstate = do
    con <- BS.readFile (head list)
    let pretty = simpleHex con 
    putStrLn pretty
    return sstate { output = pretty ++ "\n" }

-- Function for comments

comment :: Command 
comment _ sstate  = do 
                    return $ sstate { output = ""}

-- Exits hash

quit :: Command
quit _ sstate =
  return $ sstate{ output = "", wd = ";quit"}
  
-- Helping functions:

-- Parser used in echo

readStrAnything :: Parser Expr
readStrAnything =  do
    ch <- anyToken
    return $  Str [ch] 
    
-- For acquiring folder name, used in cp, mv for target directories

getFolderName :: String -> String -> String
getFolderName src target = target' ++ getFolderNameh src
  where target' = if last target /= '/' then target ++ "/" else target
        
getFolderNameh :: String -> String
getFolderNameh = reverse . takeWhile (/='/') . reverse
