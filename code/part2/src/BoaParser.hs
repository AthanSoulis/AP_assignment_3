-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>)) -- may use instead of +++ for easier portability to Parsec
import Data.Char (isDigit, isSpace, isLetter, isPrint)

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec
type ParseError = String -- you may replace this

reservedIdents = ["None", "True", "False", "for", "if", "in", "not"]

-- parseString :: String -> Either ParseError Program
-- parseString s  = case [a | (a,t) <- readP_to_S parseProgram s, all isSpace t] of
--               [a] -> Right a
--               [] -> Left "Parsing failed" 
--               _ -> Left "How did it get here ?"

parseString :: String -> Either ParseError Program
parseString s  = case [a | (a,t) <- readP_to_S parseStringConst s, all isSpace t] of
              [a] -> Right [SExp a]
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
            <|>do 
                expr <- parseExpr
                return $ SExp expr

parseIdent :: Parser String
-- parseIdent :: Parser Either VName FName
parseIdent = do
    ident <- munch1 (\x -> isDigit x || isLetter x || x == '_')
    if isDigit (head ident) || ident `elem` reservedIdents then pfail else return ident

parseExpr :: Parser Exp
parseExpr = do
                string "not"
                skipWS       -- munch1 isWhitespace
                parseExpr
            <|>
            parseRel


parseRel :: Parser Exp
parseRel = do
            e <- parseAddNeg
            parseRel' e

parseRel' :: Exp -> Parser Exp
parseRel' expr = do
                parseRelOper expr
                parseAddNeg
                <|>
                return expr

parseRelOper :: Exp -> Parser Op
parseRelOper expr = undefined 
                -- do
                -- string "=="
                -- return Eq 
                -- <|> do
                -- string "!="
                -- return Not Eq
                -- <|> do
                -- satisfy (== '<')
                -- <|> do
                -- satisfy (== '>')
                -- return 
                -- <|> do
                -- string "<="
                -- <|> do
                -- string ">="
                -- <|> do
                -- string "in"
                -- <|> do
                -- string "not in"
parseAddNeg :: Parser Exp
parseAddNeg = undefined

parseConst :: Parser Exp
parseConst = do
    --parStringConst
    parseNumConst
    <|> do
    ident <- parseIdent
    skipWS
    return $ Const (StringVal ident) --temp, fix!
    <|> do
    string "None"
    skipWS
    return $ Const NoneVal
    <|> do
    string "True"
    skipWS
    return $ Const TrueVal
    <|> do
    string "False"
    skipWS
    return $ Const FalseVal
    <|> do
    satisfy (== '(')
    skipWS
    exp <- parseExpr --undef
    skipWS
    satisfy (== ')')
    skipWS
    return exp
    <|> do --fun call syntax
    ident <- parseIdent
    munch1 isSpace
    satisfy (== '(')
    skipWS
    parseExprz  --do def
    skipWS
    satisfy (== ')')
    skipWS
    return $ Const (StringVal "temp") --temp fix
    <|> do
    satisfy (== '[')
    skipWS
    parseExpr
    skipWS
    parseForClause --do def
    skipWS
    parseClausez --do def
    skipWS
    satisfy (== ']')
    skipSpaces
    return $ Const (StringVal "temp") --temp fix
    <|> do
    satisfy (== '[')
    skipWS
    parseExprz
    skipWS
    satisfy (== ']')
    skipSpaces
    return $ Const (StringVal "temp") --temp fix

parseForClause = undefined

parseIfClause = undefined

parseClausez = undefined

parseExprz = undefined

parseExprs = undefined

parseExprs' = undefined


-- To be extended,  probably try to consume '#'  and move on from there
skipWS :: Parser ()
skipWS = skipSpaces

parseComments :: Parser ()
parseComments = undefined
        

parseNumConst :: Parser Exp
parseNumConst = do
    satisfy (== '-')
    num <- parseNumConstHelper
    return $ Const (IntVal (-num))
    <|> do
    num <- parseNumConstHelper
    return $ Const (IntVal num)

parseNumConstHelper :: Parser Int
parseNumConstHelper = do
    num <- munch1 isDigit
    skipWS
    case (head num) of
        '0' -> if length num == 1 then return 0 else pfail 
        _  -> return $ read num

parseStringConst :: Parser Exp
parseStringConst = do
    satisfy (== '\'')
    skipWS
    print <- many parseStringInside
    satisfy (== '\'')
    skipWS
    return $ Const (StringVal $ concat $ print)

parseStringInside :: Parser String
parseStringInside = do 
                    c <- satisfy isPrintable
                    return (c:[])
                    <|> do
                    string "\\\n"
                    return ""
                    <|> do
                    string "\n"
                    return "\n"
                    <|> do
                    string "\'"
                    return "\'"
                    <|> do
                    string "\\\\"
                    return "\\"


isPrintable :: Char -> Bool
isPrintable c = ((isPrint c) && (c /= '\'') && (c /= '\\'))

