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
                skipWS
                rest <- parseStmts'
                skipWS
                return (stmt:rest)

parseStmts' :: Parser [Stmt]
parseStmts' = do
    satisfy (== ';')
    skipWS
    parseStmts
    <++ return [] --doc this

parseStmt :: Parser Stmt
parseStmt = do
            ident <- parseIdent
            skipWS
            satisfy(== '=')
            skipWS
            expr <- parseExpr
            skipWS
            return $ SDef ident expr
            <|> do 
            expr <- parseExpr
            skipWS
            return $ SExp expr

-- parseExpr :: Parser Exp
-- parseExpr = do
--     skipWS          --this should be unneccessary but shouldn't hurt either
--     string "not"
--     munch1 isWhitespace  -- munch1 isWhitespace
--     ret <- parseExpr
--     return $ Not ret
--     <|> do 
--     skipWS
--     parseRel

parseExpr :: Parser Exp
parseExpr = do
    parseKeyWord "not"
    skipWS
    exp <- parseExpr
    skipWS
    return $ Not exp
    <|> do
    parseRel

parseKeyWord :: String -> Parser String
parseKeyWord str = do
    string str
    next <- look
    skipWS
    if (\x -> not (isDigit x || isLetter x || x == '_')) $ head next then return str else pfail

parseRel :: Parser Exp
parseRel = do 
            exp <- parseAddNeg
            skipWS
            parseRel' exp

parseRel' :: Exp -> Parser Exp
parseRel' expr = do
                parseRelOper expr
                <|>
                return expr

parseRelOper :: Exp -> Parser Exp
parseRelOper expr1 = do
                string "=="
                skipWS
                expr2 <- parseAddNeg
                return $ Oper Eq expr1 expr2
                <|> do
                string "!="
                skipWS
                expr2 <- parseAddNeg
                return $ Not $ Oper Eq expr1 expr2
                <|> do
                satisfy (== '<')
                skipWS
                expr2 <- parseAddNeg
                return $ Oper Less expr1 expr2
                <|> do
                satisfy (== '>')
                skipWS
                expr2 <- parseAddNeg
                return $ Oper Greater expr1 expr2
                <|> do
                string "<="
                skipWS
                expr2 <- parseAddNeg
                return $ Not $ Oper Greater expr1 expr2
                <|> do
                string ">="
                skipWS
                expr2 <- parseAddNeg
                return $ Not $ Oper Less expr1 expr2
                <|> do
                --string "in"
                --munch1 isWhitespace
                parseKeyWord "in"
                expr2 <- parseAddNeg
                return $ Oper In expr1 expr2
                <|> do
                -- string "not in"
                -- munch1 isWhitespace
                parseKeyWord "not"
                skipWS
                parseKeyWord "in"
                expr2 <- parseAddNeg
                return $ Not $ Oper In expr1 expr2

parseAddNeg :: Parser Exp
parseAddNeg = do
            m <- parseMultDiv
            skipWS
            parseAddNeg' m


parseAddNeg' :: Exp -> Parser Exp
parseAddNeg' expr = do
                    satisfy(== '+')
                    skipWS
                    m <- parseMultDiv
                    skipWS
                    parseAddNeg' $ Oper Plus expr m
                    <|> do
                    satisfy(== '-')
                    skipWS
                    m <- parseMultDiv
                    skipWS
                    parseAddNeg' $ Oper Minus expr m
                    <|>
                    return expr

parseMultDiv :: Parser Exp
parseMultDiv = do
            m <- parseConst
            skipWS
            parseMultDiv' m

parseMultDiv' :: Exp -> Parser Exp
parseMultDiv' expr = do
                    satisfy(== '*')
                    skipWS
                    m <- parseConst
                    skipWS
                    parseMultDiv' $ Oper Times expr m    
                    <|> do
                    string "//"
                    skipWS
                    m <- parseConst
                    skipWS
                    parseMultDiv' $ Oper Div expr m
                    <|> do
                    satisfy(=='%') 
                    skipWS
                    m <- parseConst
                    skipWS
                    parseMultDiv' $ Oper Mod expr m
                    <|>
                    return expr


parseConst :: Parser Exp
parseConst = do --maybe <++ instead
    parseStringConst
    <|> do
    parseNumConst
    <|> do
    ident <- parseIdent
    skipWS
    return $ Var ident
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
    exp <- parseExpr
    skipWS
    satisfy (== ')')
    skipWS
    return exp
    <|> do --fun call syntax
    fname <- parseIdent
    skipWS
    satisfy (== '(')
    skipWS
    args <- parseExprz
    skipWS
    satisfy (== ')')
    skipWS
    return $ Call fname args
    <|> do --list comp syntax
    satisfy (== '[')
    skipWS
    exp <- parseExpr
    skipWS
    for <- parseForClause
    skipWS
    rest <- parseClausez
    skipWS
    satisfy (== ']')
    skipSpaces
    return $ Compr exp (for:rest)
    <|> do --eval list syntax
    satisfy (== '[')
    skipWS
    exprz <- parseExprz
    skipWS
    satisfy (== ']')
    skipSpaces
    return $ List exprz

parseForClause :: Parser CClause
parseForClause = do
    -- string "for"
    -- munch1 isWhitespace --isWhitespace #
    parseKeyWord "for"
    ident <- parseIdent
    skipWS
    -- string "in"
    -- munch1 isWhitespace --isWhitespace #
    parseKeyWord "in"
    exp <- parseExpr
    skipWS
    return $ CCFor ident exp

parseIfClause :: Parser CClause
parseIfClause = do
    -- string "if"
    -- munch1 isWhitespace -- isWhitespace
    parseKeyWord "if"
    exp <- parseExpr
    return $ CCIf exp

parseClausez :: Parser [CClause]
parseClausez = do
    for <- parseForClause
    rest <- parseClausez
    return (for:rest)
    <|> do
    iff <- parseIfClause
    rest <- parseClausez
    return (iff:rest)
    <++ return [] --doc this

parseExprz :: Parser [Exp]
parseExprz = do parseExprs; 
            <++ return []   --doc this

parseExprs :: Parser [Exp]
parseExprs = do
    exp <- parseExpr
    skipWS
    rest <- parseExprs'
    return (exp:rest)

parseExprs' :: Parser [Exp]
parseExprs' = do
    satisfy (== ',')
    skipWS
    parseExprs
    <++ return [] --doc this

skipWS :: Parser ()
skipWS = do 
    skipSpaces
    satisfy (== '#')
    skipComments
    <++ do  --doc this
    skipSpaces

skipComments :: Parser ()
skipComments = do
    munch (/= '\\')
    skipCommentsEnd

skipCommentsEnd :: Parser ()
skipCommentsEnd = do
    eof
    <++ do --doc this
    string "\n"
    skipSpaces
    return mempty
        
parseIdent :: Parser String
parseIdent = do
    ident <- munch1 (\x -> isDigit x || isLetter x || x == '_')
    skipWS
    if isDigit (head ident) || ident `elem` reservedIdents then pfail else return ident

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
    case head num of
        '0' -> if length num == 1 then return 0 else pfail 
        _  -> return $ read num

parseStringConst :: Parser Exp
parseStringConst = do
    satisfy (== '\'')
    print <- many parseStringInside
    satisfy (== '\'')
    skipWS
    return $ Const (StringVal $ concat print)

parseStringInside :: Parser String
parseStringInside = do 
                    c <- satisfy isPrintable
                    return [c]
                    <|> do
                    string "\\\n"
                    return ""
                    <|> do
                    string "\\n"
                    return "\n"
                    <|> do
                    string "\\\'"
                    return "\'"
                    <|> do
                    string "\\\\"
                    return "\\"


isPrintable :: Char -> Bool
isPrintable c = isPrint c && (c /= '\'') && (c /= '\\')

--extend to #
-- isWhitespace :: Char -> Bool
-- isWhitespace = isSpace

