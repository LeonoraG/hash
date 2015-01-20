-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.
 module Hash.Parsing.HashParser where
 
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad

import Hash.Language.Expressions
import Text.ParserCombinators.Parsec


err = "An error has occurred"

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse p err

-- expects syntax : if (something without spaces) then (something without spaces) fi

parseIf :: Parser [String]
parseIf = do 
          string "if "
          cond <- many (noneOf " ")
          string " then "
          cthen <- many (noneOf " ")
          string " fi"
          return $ [cond,cthen]

-- expects syntax : if (something without spaces) then (something without spaces) else (something without spaces) fi

parseIfElse :: Parser [String]
parseIfElse = do
              string "if "
              cond <- many (noneOf " ")
              string " then "
              cthen <- many (noneOf " ")
              string " else "
              celse <- many (noneOf " ")
              string " fi"
              return $ [cond,cthen,celse]
              
--parses a contition : something <,>,=, <=, /= something

parseCond :: Parser [String]
parseCond = do
            first <- many (noneOf "<>=/")
            comp <- many (oneOf "<>=/")
            second <- many (noneOf "<>=/")
            return [first,comp,second]


