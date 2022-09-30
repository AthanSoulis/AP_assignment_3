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

--parses a boa program according to the modified grammar in BNF_grammar.txt
--using parser combinators allows us to combine both lexical and syntactical analysis
parseString :: String -> Either ParseError Program
parseString s  = case [a | (a,t) <- readP_to_S parseProgram s, all isSpace t] of
              [a] -> Right a
              [] -> Left "Parsing failed" 
              _ -> Left "How did it get here ?"

parseProgram :: Parser Program
parseProgram = do
                skipWS
                parseStmts

--skipWS (skips whitespace and comments)
--is used overly cautious all throughout our code
--many of these are superflous since every parser should
--already skip all the WS after it's token
--(and parseProgramm at the very beginning)
--nonetheless, this ensures a correct parse
--and is only a minor drawback with regards to efficiency
parseStmts :: Parser [Stmt]
parseStmts = do 
                stmt <- parseStmt
                --skipWS --redundant
                rest <- parseStmts'
                return (stmt:rest)

--epsilon production handled last
parseStmts' :: Parser [Stmt]
parseStmts' = do
    satisfy (== ';')
    skipWS
    parseStmts
    <|> do
    skipWS
    return []

parseStmt :: Parser Stmt
parseStmt = do
            ident <- parseIdent
            satisfy(== '=')
            skipWS
            expr <- parseExpr
            return $ SDef ident expr
            <|> do 
            expr <- parseExpr
            return $ SExp expr


parseExpr :: Parser Exp
parseExpr = do
    parseKeyWord "not"
    exp <- parseExpr
    return $ Not exp
    <|> do
    parseRel

--this function parses keywords like not, in etc...
--we use look to peak at the letter following the
--keyword without consuming it.
--if this letter could belong to an identifier
--then we can not parse a keyword like for ex. in "notx"
--this is necessary bc "not(x)" should parse to an Expr
parseKeyWord :: String -> Parser String
parseKeyWord str = do
    string str
    next <- look
    skipWS
    if (\x -> not (isDigit x || isLetter x || x == '_')) $ head next then return str else pfail

parseRel :: Parser Exp
parseRel = do 
            exp <- parseAddNeg
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
                parseKeyWord "in"
                expr2 <- parseAddNeg
                return $ Oper In expr1 expr2
                <|> do
                parseKeyWord "not"
                skipWS
                parseKeyWord "in"
                expr2 <- parseAddNeg
                return $ Not $ Oper In expr1 expr2

parseAddNeg :: Parser Exp
parseAddNeg = do
            m <- parseMultDiv
            parseAddNeg' m

parseAddNeg' :: Exp -> Parser Exp
parseAddNeg' expr = do
                    satisfy(== '+')
                    skipWS
                    m <- parseMultDiv
                    parseAddNeg' $ Oper Plus expr m
                    <|> do
                    satisfy(== '-')
                    skipWS
                    m <- parseMultDiv
                    parseAddNeg' $ Oper Minus expr m
                    <|>
                    return expr

parseMultDiv :: Parser Exp
parseMultDiv = do
            m <- parseConst
            parseMultDiv' m

parseMultDiv' :: Exp -> Parser Exp
parseMultDiv' expr = do
                    satisfy(== '*')
                    skipWS
                    m <- parseConst
                    parseMultDiv' $ Oper Times expr m    
                    <|> do
                    string "//"
                    skipWS
                    m <- parseConst
                    parseMultDiv' $ Oper Div expr m
                    <|> do
                    satisfy(=='%') 
                    skipWS
                    m <- parseConst
                    parseMultDiv' $ Oper Mod expr m
                    <|>
                    return expr

--the last to productions in this expression
--namely List and List Comprehnsion expressions
--cause some major efficieny issues when
--parsing deep brackets. this is because both
--productions start with the same symbol
--and due to the nature of built in backtracking
--in readP we have to reevaluate (parts) of the
--input several times skipping back and forth
--a remedy might exist using the <++ operator
parseConst :: Parser Exp
parseConst = do
    parseStringConst
    <|> do
    parseNumConst
    <|> do
    ident <- parseIdent
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
    satisfy (== ')')
    skipWS
    return exp
    <|> do --fun call syntax
    fname <- parseIdent
    satisfy (== '(')
    skipWS
    args <- parseExprz
    satisfy (== ')')
    skipWS
    return $ Call fname args
    <|> do --eval list syntax
    satisfy (== '[')
    skipWS
    exprz <- parseExprz
    satisfy (== ']')
    skipSpaces
    return $ List exprz
    <|> do --list comp syntax
    satisfy (== '[')
    skipWS
    exp <- parseExpr
    for <- parseForClause
    rest <- parseClausez
    satisfy (== ']')
    skipSpaces
    return $ Compr exp (for:rest)

parseForClause :: Parser CClause
parseForClause = do
    parseKeyWord "for"
    ident <- parseIdent
    parseKeyWord "in"
    exp <- parseExpr
    return $ CCFor ident exp

parseIfClause :: Parser CClause
parseIfClause = do
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
    <|> return []

parseExprz :: Parser [Exp]
parseExprz = do parseExprs; 
            <|> return []

parseExprs :: Parser [Exp]
parseExprs = do
    exp <- parseExpr
    rest <- parseExprs'
    return (exp:rest)

parseExprs' :: Parser [Exp]
parseExprs' = do
    satisfy (== ',')
    skipWS
    parseExprs
    <|> return []

--just skipSpaces should only be applied
--if we have no comments to skip
--hence, the <++ operator is used to
--only check this option if no # could be found
skipWS :: Parser ()
skipWS = do 
    skipSpaces
    satisfy (== '#')
    skipComments
    <++ do  --doc this
    skipSpaces

skipComments :: Parser ()
skipComments = do
    munch (/= '\n')
    skipCommentsEnd

--a comment either ends with \n or
--at the end of the input
skipCommentsEnd :: Parser ()
skipCommentsEnd = do
    eof
    <|> do
    string "\n"
    skipWS
    return mempty

--the following functions parse the complex terminals
--according to the specifications   
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