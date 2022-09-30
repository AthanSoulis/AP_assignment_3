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
  
  testCase "Nested not" $
    case parseString "not (not (x < 3))" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),

  testCase "Minus Zero" $
    case parseString "-0" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),

  testCase "Hundo" $
    case parseString "100" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),

  testCase "Agent 007" $
    case parseString "007" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse this: " ++ show p]

numConst = testGroup "Numerical Constants tests" []

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
      Right p -> return ()
      ]

tokenWS = testGroup "Whitespace around tokens" []

comments = testGroup "Comment tests" []

disambiguation = testGroup "Disambiguation tests" []

conc_abs_syntax = testGroup "Concrete and abstract syntax correspondence tests" [
  testCase "Empty program" $
    case parseString "" of
      Left e -> return ()
      Right p -> assertFailure $ "Should not parse empty program"
      ]

generalTests = testGroup "General tests" [

    testCase "Parse big brackets" $
    case parseString "[[[[[[[[[[[[[[[[[[[[x]]]]]]]]]]]]]]]]" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return (),
    testCase "Parse big parentheses" $
    case parseString "((((((((((((((((((((x))))))))))))))))))))" of
      Left e -> assertFailure $ "Error: " ++ e
      Right p -> return ()
      ]

