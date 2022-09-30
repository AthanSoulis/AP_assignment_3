-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Test Suite" [min_tests, identifiers, numConst, stringConst,
 tokenWS, comments, disambiguation, conc_abs_syntax, generalTests]

min_tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

identifiers = testGroup "Identifier tests" [

  testCase "keyword identifier" $
    parseString "notx1_" @?=
      Right [SExp (Var "notx1_")],

  testCase "illegal identifier" $
    case parseString "1z" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse this: " ++ show p
    ]

numConst = testGroup "Numerical Constants tests" [

  testCase "Zero" $
    parseString "0" @?=
      Right [SExp (Const (IntVal 0))],

  testCase "Hundo" $
    parseString "-1337" @?=
      Right [SExp (Const (IntVal (-1337)))],

  testCase "Agent 007" $
    case parseString "007" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse this: " ++ show p]

stringConst = testGroup "String Constants tests" [
  testCase "a\"b\\n" $
    parseString "'a\"b\\n'" @?=
      Right [SExp (Const (StringVal "a\"b\n"))],

  testCase "Parse backslash illegal" $
    case parseString "'fo\\o'" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse this: " ++ show p,
      
  testCase "Parse double backslash" $
    case parseString "'fo\\\\o'" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),

  testCase "New lines" $
    parseString "'Stay, a while \\n\\n and listen'" @?=
      Right [SExp (Const (StringVal "Stay, a while \n\n and listen"))],
  
  testCase "New lines err" $
      case parseString "'Stay, a while \n\n and listen'" of
        Left e -> return ()
        Right p -> assertFailure $ "Should not parse: " ++ show p,

  testCase "Exclamation, change line character" $
    case parseString "'-Elrond: Destroy it!!!! ISILDUUUUR! \\n-Isildur: No.'" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return ()
      ]

tokenWS = testGroup "Whitespace around tokens" [
    
    testCase "x == y" $
    parseString "x  ==(   y)   " @?=
      Right [SExp (Oper Eq (Var "x") (Var "y"))],

    testCase "x in y" $
    parseString "x\tin(\ny)" @?=
      Right [SExp (Oper In (Var "x") (Var "y"))],

    testCase "Illegal new line" $
    case parseString "' \        \n'" of
      Left e -> return () 
      Right p -> assertFailure $ "Should not parse: " ++ show p
      ]

comments = testGroup "Comment tests" [

    testCase "not comment" $
    parseString "not#comment\\\ncool" @?=
      Right [SExp (Not (Var "cool"))],

    testCase "mult comment" $
    parseString "#c\\\n   #\\\ncool #abc\\\n #sds" @?=
      Right [SExp (Var "cool")],

    testCase "comment not closed" $
    case parseString "#abc not x" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p  
      ]  

disambiguation = testGroup "Disambiguation tests" [
  testCase "Nested not" $
      case parseString "not (not (x < 3))" of
        Left e -> assertFailure $ "Error: " ++ e
        Right p -> return (),

  testCase "associativity1" $
    case parseString "x < y >= z" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse empty program",

  testCase "associativity2" $
      parseString "x-y+z" @?=
      Right [SExp (Oper Plus (Oper Minus (Var "x") (Var "y")) (Var "z"))],  

  testCase "associativity3" $
      parseString "4*3%2" @?=
      Right [SExp (Oper Mod (Oper Times (Const (IntVal 4)) (Const (IntVal 3))) (Const (IntVal 2)))]   
      ]

conc_abs_syntax = testGroup "Concrete and abstract syntax correspondence tests" [
  testCase "Empty program" $
    case parseString "" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse empty program",

  testCase "not eq" $
    parseString "x != 2" @?=
      Right [SExp (Not (Oper Eq (Var "x") (Const (IntVal 2))))],

  testCase "greater than" $
      parseString "4 >= 42" @?=
      Right [SExp (Not (Oper Less (Const (IntVal 4)) (Const (IntVal 42))))],

  testCase "list" $
      parseString "[x==2, 1]" @?=
      Right [SExp (List [Oper Eq (Var "x") (Const (IntVal 2)),Const (IntVal 1)])]
      ]

generalTests = testGroup "General tests" [

    testCase "Parse medium brackets" $
    case parseString "[[[[x]]]]" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),

    testCase "Parse big brackets" $
    case parseString "[[[[[[[[[[[[[[[[[[[[x]]]]]]]]]]]]]]]]" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),
    testCase "Parse big parentheses 1" $
    case parseString "((((((((((((((((((((x))))))))))))))))))))" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),
    testCase "Parse big parentheses 2" $
    case parseString "(((((((((((((((((((s)))))))))))+((((((((((((x))))))))))))))))))))" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),
    testCase "Bracket stmts" $
    case parseString "(s;x)" of
      Left e ->  return ()
      Right p -> assertFailure $ "Should not parse: " ++ show p,

    testCase "Statements" $
      parseString "x==2;\nprint(x, 42)" @?=
        Right [SExp (Oper Eq (Var "x") (Const (IntVal 2))),SExp (Call "print" [Var "x",Const (IntVal 42)])],

    testCase "reserved" $
      parseString "None; True; False" @?=
        Right [SExp (Const NoneVal),SExp (Const TrueVal),SExp (Const FalseVal)],
  
    testCase "List Compr" $
      parseString "[x for x in y if x < 3]" @?=
        Right [SExp (Compr (Var "x") [CCFor "x" (Var "y"),CCIf (Oper Less (Var "x") (Const (IntVal 3)))])]]

