-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

import Data.Char (isDigit, isSpace)

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec
type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
-- parseString = undefined  -- define this
-- parseString s = case [a | (a,t) <- readP_to_S parseNumConst s, all isSpace t] of
--               [a] -> Right [SExp a]
--               [] -> Left "Parsing failed"
--               _ -> Left "How did it get here ?"

-- parseIdent :: Parser 




parseNumConst :: Parser Exp
-- parseNumConst = do
--     satisfy (== '-')
--     parseNumConstHelper true
--     <|> do
--     parseNumConstHelper false
--     where parseNumConstHelper minus = do
--         num <- munch1 isDigit
--         case (head num) of
--             '0' -> if length num == 1 then return $ Const (IntVal 0) else pfail 
--             _  -> return $ Const (IntVal if minus then read ("-"++num) else read num)
    

parseNumConst = do
    num <- munch1 isDigit
    case (head num) of
        '0' -> if length num == 1 then return $ Const (IntVal 0) else pfail 
        _  -> return $ Const (IntVal $ read num)
    <|> do 
    satisfy (== '-')
    num <- munch1 isDigit
    case (head num) of
        '0' -> if length num == 1 then return $ Const (IntVal 0) else pfail
        _   -> return $ Const (IntVal $ read ("-"++num))