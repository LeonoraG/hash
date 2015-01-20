-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. 

 module Hash.Parsing.HashParser where
 
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), Applicative)
import Hash.Language.Expressions


err = "An error has occurred"

cmndParse :: Parser a -> String -> Either ParseError a
cmndParse p = parse p err


-- Parser for expression

parseExpr :: Parser Expr
parseExpr =  parseExprVar <|> parseExprStr

parseExprVar :: Parser Expr
parseExprVar = do
               char '$'
               first <- letter
               rest  <- many alphaNum
               return $ Var (first:rest)
             
parseExprStr :: Parser Expr
parseExprStr = do        
    x <- many1 (readEscapedChar <|> readEncloseString <|> many1 (noneOf "\" \\\n\t") )
    return $ Str $ concat x
    
readEncloseString :: Parser String
readEncloseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return x
 
readEscapedChar :: Parser String
readEscapedChar = do
    char '\\'
    a <- anyChar
    return [a]
    
-- Parsers for comparison

parseComp :: Parser Comp
parseComp = parseSingleC <|> parseC

parseSingleC :: Parser Comp
parseSingleC = CLI <$> parseExpr

parseC :: Parser Comp
parseC = do
            a <- parseExpr
            symb <- choice $ fmap (try.string) $ ["==","/=", ">", ">=","<","<=",""] 
            b <- parseExpr
            return $ case symb of
              "==" ->  CEQ a b
              "/=" ->  CNE a b
              ">=" ->  CGE a b
              ">"  ->  CGT a b
              "<=" ->  CLE a b
              "<"  ->  CLT a b

              
-- Parsers for predicates
parsePred :: Parser Pred
parsePred =  try parseNot <|> try parseAnd <|> try parseOr <|> try parseParens <|> parseSingleP

parseSingleP :: Parser Pred
parseSingleP = Pred <$> parseComp

parseParens :: Parser Pred
parseParens = do
              char '('
              e <- parsePred
              char ')'
              return $ Parens e
              
parseNot :: Parser Pred           
parseNot = do
           string "Not"
           p <- parsePred
           return $ Not p
           
parseAnd :: Parser Pred
parseAnd = do
           a <- parseParens
           string "And"
           b <- parseParens
           return $ And a b
           
parseOr :: Parser Pred
parseOr = do
           a <- parseParens
           string "Or"
           b <- parseParens
           return $ Or a b
          
-- Parser for commands

parseCmnd :: Parser Cmd
parseCmnd = try parseCmd <|> try parseAsgn <|> try parseCmdRedirIn <|> try parseCmdRedirOut

parseAsgn :: Parser Cmd
parseAsgn = do
           spaces
           var' <- parseExprVar
           spaces
           char '='
           spaces
           val' <- parseExpr
           return $ Assign { var = var', val = val'}
    
-- Ignoring the redirection

parseCmd :: Parser Cmd
parseCmd = do
           spaces
           fname <- parseExpr
           spaces
           arguments <- sepBy parseExpr (many $ char ' ' <|> char '\t')
           optional $ char '\n'
           return Cmd { name   = fname 
                      , args   = arguments 
                      , inDir  = Nothing 
                      , outDir = Nothing 
                      , append = False
                      }
                      
 -- Redirected input
 
parseCmdRedirIn :: Parser Cmd
parseCmdRedirIn = do
           spaces
           fname <- parseExpr
           spaces
           arguments <- sepBy parseExpr (many $ char ' ' <|> char '\t')
           spaces
           char '<'
           path <- parseExpr
           optional $ char '\n'
           return Cmd { name   = fname 
                      , args   = arguments 
                      , inDir  = Just path
                      , outDir = Nothing 
                      , append = False
                      }
                      
-- Redirected output

parseCmdRedirOut :: Parser Cmd
parseCmdRedirOut = do
           spaces
           fname <- parseExpr
           spaces
           arguments <- sepBy parseExpr (many $ char ' ' <|> char '\t')
           spaces
           x <- many (oneOf ">")
           spaces
           path <- parseExpr
           optional $ char '\n'
           return Cmd { name   = fname 
                      , args   = arguments 
                      , inDir  = Nothing 
                      , outDir = Just path 
                      , append = x == ">>"
                      }

                      

-- Parsers for conditionals

parseCond :: Parser Conditional
parseCond = try parseIf <|> try parseIfElse

parseIf :: Parser Conditional
parseIf = do 
          string "if"
          spaces
          cond' <- parsePred
          spaces
          string "then"
          spaces
          then' <- sepBy parseCmnd (many $ char ' ')
          spaces
          string "fi"
          return $ If { cond = cond'
                      , cthen = then'
                      }
          
parseIfElse :: Parser Conditional
parseIfElse = do
              string "if"
              spaces
              cond' <- parsePred
              spaces
              string "then"
              spaces
              cthen' <- sepBy parseCmnd (many $ char ' ')
              spaces
              string "else"
              spaces
              celse' <- sepBy parseCmnd (many $ char ' ')
              spaces
              string "fi"
              return $ IfElse { cond  = cond'
                              , cthen = cthen' 
                              , celse = celse'
                              }

-- Top level expression parser

parseTLExpr :: Parser TLExpr
parseTLExpr = try isCond <|> isntCond

isCond   = TLCnd <$> parseCond
isntCond = TLCmd <$> parseCmd


