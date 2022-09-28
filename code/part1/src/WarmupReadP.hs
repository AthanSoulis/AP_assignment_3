module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- E ::= TF | "-" TF .
-- F::= "+" TF | "-" TF | ε .
-- T ::= num | "(" E ")" .

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

import Data.Char (isDigit, isSpace)

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s = case [a | (a,t) <- readP_to_S parseE s, all isSpace t] of
              [a] -> Right a
              [] -> Left "Parsing failed" 
              _ -> Left "How did it get here ?"

parseE :: Parser Exp
parseE = do 
          t <- parseT; 
          parseW;
          parseF t;
        <|>
          do 
            parseW;
            satisfy (== '-'); 
            parseW;
            t <- parseT;
            parseW;
            parseF $ Negate t;

parseF :: Exp -> Parser Exp
parseF expr = 
        do 
          parseW;
          satisfy (== '+'); 
          parseW;
          t <- parseT;
          parseW;
          f <- parseF $ Add expr t;
          parseW;
          return f
        <|>
          do 
            parseW;
            satisfy (== '-'); 
            parseW;
            t <- parseT;
            parseW;
            parseF $  Add expr $ Negate t
          <|>
            return expr


parseT :: Parser Exp
parseT = do 
          parseW
          num <-parseNum;
          return $ Num num  
        <|>
        do 
          satisfy (== '(');
          parseW;
          t <- parseE;
          parseW;
          satisfy (== ')');
          return t 

parseNum :: Parser Int
parseNum = do 
            nums <- munch1 isDigit
            return $ read nums

-- ReadP.skipSpaces can be substituted by any function that ignores spaces
parseW :: Parser ()
parseW = skipSpaces
