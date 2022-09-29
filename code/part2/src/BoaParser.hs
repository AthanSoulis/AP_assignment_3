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
parseString s  = case [a | (a,t) <- readP_to_S parseProgram s, all isSpace t] of
              [a] -> Right a
              [] -> Left "Parsing failed" 
              _ -> Left "How did it get here ?"


parseProgram :: Parser Program
parseProgram = do
                skipWS
                parseStmts

parseStmts :: Parser [Stmt]
parseStmts = do 
                stmt <- parseStmt
                parseStmts' [stmt]

parseStmts' :: [Stmt] -> Parser [Stmt]
parseStmts' prevStmts = do
                            satisfy(==';')
                            skipWS
                            parseStmts' prevStmts
                        <|>
                            return prevStmts

parseStmt :: Parser Stmt
parseStmt = do
                ident <- parseIdent
                skipWS
                satisfy(=='=')
                skipWS
                expr <- parseExpr
                return $ SExp expr
            <|>
                do 
                    expr <- parseExpr
                    return $ SExp expr

parseIdent :: Parser String
-- parseIdent :: Parser Either VName FName
parseIdent = undefined

parseExpr :: Parser Exp
parseExpr = do
                satisfy(=='n')
                -- satisfy(=="not") 
                skipWS
                parseExpr
            <|>
                parseRel


parseRel :: Parser Exp
parseRel = do
            e <- parseAddNeg
            parseRel' e

parseRel' :: Exp -> Parser Exp
parseRel' expr = do
                parseRelOper
                parseAddNeg
            <|>
                return expr

parseRelOper :: Parser Op
parseRelOper = undefined

parseAddNeg :: Parser Exp
parseAddNeg = undefined

parseConst :: Parser Exp
parseConst = undefined

-- To be extended,  probably try to consume '#'  and move on from there
skipWS :: Parser ()
skipWS = skipSpaces
        



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