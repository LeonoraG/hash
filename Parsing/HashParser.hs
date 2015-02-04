-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs.

module Parsing.HashParser where

import Control.Monad
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Data.List
import Language.Expressions
import Text.ParserCombinators.Parsec

-- Expr is a bottom-level expression
-- It can be either a named variable or a string

parseExpr :: Parser Expr
parseExpr =  try varExp <|> strExpr

-- Using usual syntax for variables : $...

varExp :: Parser Expr
varExp = do
    char '$'
    first <- letter
    rest  <- many alphaNum
    return $ Var (first:rest)

-- Tries to read enclosed, then escaped string
-- If neither of this works, reads many characters that don't contain
-- some special chars

strExpr :: Parser Expr
strExpr = do         
    x <- many1 (escapedChar<|>enclosedString<|>many1 (noneOf "\" \\\n\t;") ) 
    return $ Str $ concat x
    
-- For strings between "
enclosedString :: Parser String
enclosedString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return x

-- For strings that begin with \\
escapedChar :: Parser String
escapedChar = do
    char '\\'
    a <- anyChar
    return [a]
 

-- Comparison parser

parseComp :: Parser Comp
parseComp = try compExpr <|> wrappedExpr

-- When we have a wrapped expression literal
wrappedExpr :: Parser Comp
wrappedExpr = liftM CLI $ parseExpr
    

compExpr :: Parser Comp
compExpr = do
    x <- parseExpr
    operator <-choice $ fmap (try.string) $ ["==","/=", ">", ">=",">","<=",""] 
    y <- parseExpr
    return $ case operator of
              "==" ->  CEQ x y
              "/=" ->  CNE x y
              ">=" ->  CGE x y
              ">"  ->  CGT x y
              "<=" ->  CLE x y
              "<"  ->  CLT x y
              
-- Parsing predicates

parsePred :: Parser Pred
parsePred = try parensPred <|> try andOrPred <|> try notPred <|> wrappedComp

notPred :: Parser Pred
notPred =  char '!' >> (liftM Not $ parsePred)

andOrPred :: Parser Pred
andOrPred = do
    a <- try parensPred <|> wrappedComp
    symb <- choice $ fmap (try.string) $ ["&&", "||"]
    b <- try parensPred <|> wrappedComp
    return $ case symb of
                  "&&" -> And a b
                  "||" -> Or  a b

parensPred :: Parser Pred
parensPred = do
  char '(' 
  a <- parsePred
  char ')'
  return $ Parens a
  
-- For a wrapped comparison

wrappedComp :: Parser Pred
wrappedComp = liftM Pred $ parseComp
  
-- Commands can be either Cmd or an assingment

parseCmd = try assign <|> realCmd  

-- Assingns: var1 = val1
assign :: Parser Cmd 
assign = do
    spaces
    var1 <- varExp
    char '='
    val1 <- parseExpr
    spaces
    optional $ char ';' 
    return $ Assign { var = var1, val = val1}

-- Real command consists of name and a list of arguments
-- arguments can be separated by space or tab

realCmd :: Parser Cmd 
realCmd = do
    spaces
    name' <- parseExpr
    spaces
    args_ <- sepBy parseExpr (many $ char ' ' <|> tab)
    spaces
    let (args', inDir', outDir', append') = handleRedirects args_
    spaces
    --optional newline
    optional $ char ';' 
    return $ Cmd { name   = name'
                 , args   = args'
                 , inDir  = inDir'
                 , outDir = outDir'
                 , append = append'
                 }
   
-- Handling redirects
handleRedirects :: [Expr] -> ( [Expr], Maybe Expr, Maybe Expr, Bool)
handleRedirects args = outRedir $ inRedir args

rmvIndexes :: [a] -> Int -> Int -> [a]
rmvIndexes list arg1 arg2 = map snd $ filter (\x -> fst x /= arg1 && fst x /= arg2) $ indL
  where indL = zip [0..] list

inRedir :: [Expr] -> (Maybe Expr, [Expr])
inRedir args = case findIndices (==Str "<") args of
                    [] -> (Nothing, args)
                    a  -> (Just $ args !! (last a + 1), rmvIndexes args (last a) (last a +1) )
                   
outRedir :: (Maybe Expr, [Expr]) -> ( [Expr], Maybe Expr, Maybe Expr, Bool)
outRedir (inRedir, args) = case findIndices (== Str ">") args of
                              [] -> case findIndices (== Str ">>") args of
                                      [] -> (args, Nothing, inRedir, False)
                                      a  -> (rmvIndexes args (last a) (last a + 1), inRedir, Just $ args !! (last a + 1) , True)
                              a  -> (rmvIndexes args (last a) (last a + 1), inRedir, Just $ args !! (last a + 1) , False)
                              

                              
-- Parses conditional branching expressions :
-- if-then or if-then- else

-- The syntax is following:
-- ifThenElse: if condition , then expression1;;expression2;, else expression3;;expression4;, fi
-- ifThen:     if condition , then expression1;;expression2;, fi

parseConditional :: Parser Conditional
parseConditional = try ifThenElse <|> ifThen


ifThen :: Parser Conditional
ifThen = do
    spaces
    string "if"
    spaces
    cond2 <- parsePred
    spaces
    string ", then"
    spaces
    then2 <- (try parseCmd) `sepBy` (symbol ';')   
    string ", fi"                                   
    return $ If { cond  = cond2
                , cthen = then2
                }
    
ifThenElse :: Parser Conditional
ifThenElse = do
    spaces
    string "if"   
    spaces
    cond2 <- parsePred
    spaces
    string ", then"  
    optional newline
    then2 <- (try parseCmd) `sepBy` (symbol ';')     
    spaces    
    string ", else"
    spaces
    else2 <- (try parseCmd) `sepBy` (symbol ';') 
    spaces
    string ", fi"
    return $ IfElse { cond  = cond2
                    , cthen = then2
                    , celse = else2
                    }
                    
token' :: Parser a -> Parser a
token' = (<* spaces)

symbol :: Char -> Parser Char
symbol = token' . char

-- Parses a top level expression, which can be either
-- conditional, or regular command

parseTLExpr :: Parser TLExpr
parseTLExpr = (TLCnd <$> try parseConditional) <|> (TLCmd <$> try parseCmd)

exprFromFile :: String -> IO (Either ParseError [Expr])
exprFromFile fp = parseFromFile (sepBy parseExpr (many $ char ' ' <|> char '\t')) fp

tLExprsFromFile :: String -> IO (Either ParseError [TLExpr])
tLExprsFromFile fp = parseFromFile (many $ skipMany parseComment >> parseTLExpr <* skipMany parseComment) fp

parseTLExprsFromFile :: FilePath -> [String] -> IO [TLExpr]
parseTLExprsFromFile fp args = do
    con <- readFile fp --open the file 
    let fline = head $ lines con --take the first line
    let vars = parse (endBy varExp spaces) "variables" fline 
    let (takesVars, varExprsList) = case vars of 
                     Left _  -> (False, [])
                     Right xs -> (True,  xs)
    let body = case takesVars of
                True  -> unlines $ tail $ lines con
                False -> con
    let extraTL = case takesVars of
                True  -> map (\(v,arg) -> TLCmd $ Assign {var = v, val = Str arg} ) $ zip varExprsList args
                False -> []
    let ret = parse readTLExprAndComments "read script" body
    return $ case ret of
         Left  e -> error "Malformed script!"
         Right tls -> extraTL ++ tls
    
readTLExprAndComments = endBy parseTLExpr spaces

-- Parses out comments 
-- comments are strings that begin with #

parseComment :: Parser ()
parseComment =  spaces >> char '#' >> many (noneOf "\n") >> optional newline >> return ()
