-- Skeleton file for Boa Interpreter. 
--Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  m >>= f  = Comp (\env -> case runComp m env of 
              (Left e, out1)  -> (Left e, out1)
              (Right a, out1) -> case runComp (f a) env of
                (Left e2, out2)  -> (Left e2, out1 ++ out2)
                (Right x, out2) -> (Right x, out1 ++ out2))
                

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\_ -> (Left re, mempty))

look :: VName -> Comp Value
look x = Comp (\env -> case lookup x env of
          Nothing -> (Left (EBadVar x), mempty)
          Just y  -> (Right y, mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp (\env -> case runComp m ((x, v):env) of
                      (Left e, out)  -> (Left e, out)
                      (Right a, out) -> (Right a, out))

output :: String -> Comp ()
output [] = Comp (\_ -> (Right (), []))
output s = Comp (\_ -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal        = False
truthy FalseVal       = False
truthy (IntVal 0)     = False
truthy (StringVal "") = False
truthy (ListVal [])   = False
truthy _              = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus    (IntVal v1) (IntVal v2) = 
  Right (IntVal (v1 + v2))
operate Minus   (IntVal v1) (IntVal v2) = 
  Right (IntVal (v1 - v2))
operate Times   (IntVal v1) (IntVal v2) = 
  Right (IntVal (v1 * v2))
operate Div     (IntVal v1) (IntVal v2) = 
  if v2 /= 0 then Right (IntVal (v1 `div` v2)) 
  else Left $ "Attempted division by zero in " 
    ++ show v1 ++ " `div` " ++ show v2
operate Mod     (IntVal v1) (IntVal v2)       = 
  if v2 /= 0 then Right (IntVal (v1 `mod` v2))
  else Left $ "Attempted division by zero in " 
    ++ show v1 ++ " `mod` " ++ show v2
operate Eq      (IntVal v1) (IntVal v2)       = 
  Right (if v1 == v2 then TrueVal else FalseVal)
operate Eq      (StringVal v1) (StringVal v2) = 
  Right (if v1 == v2 then TrueVal else FalseVal)
operate Eq      (ListVal v1) (ListVal v2)     = 
  Right (if v1 == v2 then TrueVal else FalseVal)
operate Eq       v1 v2                        = 
  Right (if v1 == v2 then TrueVal else FalseVal)
operate Less    (IntVal v1) (IntVal v2)       = 
  Right (if v1 < v2 then TrueVal else FalseVal)
operate Greater (IntVal v1) (IntVal v2)       = 
  Right (if v1 > v2 then TrueVal else FalseVal)
operate In       _ (ListVal [])               = 
  Right FalseVal
operate In       v1 (ListVal (v:vs))          = 
  case operate Eq v1 v of 
  (Left _) -> Right FalseVal
  (Right x) -> if x == TrueVal then Right TrueVal 
    else operate In v1 (ListVal vs)
operate o v1 v2 = Left $ "Operator " ++ show o 
  ++ " is applied to inappropriate arguments " 
  ++ show v1 ++ ", " ++ show v2


apply :: FName -> [Value] -> Comp Value
apply "range" [n2] = apply "range" [IntVal 0, n2, IntVal 1]
apply "range" [n1, n2] = apply "range" [n1, n2, IntVal 1]
--lots of duplicate code, this can surely be done in a neater way
apply "range" [IntVal n1, IntVal n2, IntVal n3]
  | n3 == 0 = abort $ EBadArg (show n3)
  | n3 > 0  = if n1 >= n2 then return $ ListVal [] else 
    return 
      (ListVal $ map IntVal (takeWhile (< n2) (iterate (+ n3) n1)))
  | n3 < 0  = if n1 <= n2 then return $ ListVal [] else 
    return
      (ListVal $ map IntVal (takeWhile (> n2) (iterate (+ n3) n1)))
apply "range" x = abort $ EBadArg (show x)

-- doesnt work bc output results in comp () and we want comp value
-- apply "print" x = output out where
--   out = if x /= [] then tail $ concatMap toString x else ""
apply "print" x = Comp (\_ -> (Right NoneVal, out)) where
  out = if x /= [] then [tail $ concatMap toString x] else [""]


-- apply "print" x = do 
--   output (tail $ concatMap toString x)
--   return NoneVal
--  where
--   out = if x /= [] then [tail $ concatMap toString x] else []

apply x _ = abort (EBadFun x)

--basic string conversion
--every convertet string starts with a whitespace 
--to seperate it in the above list
--the leading whitespace is ignored by using tail
toString :: Value -> String
toString NoneVal = " None"
toString TrueVal = " True"
toString FalseVal = " False"
toString (IntVal n) = " " ++ show n
toString (StringVal s) = " " ++ s
toString (ListVal []) = " []"
toString (ListVal xs) = " [" ++ tail 
  (tail $ concatMap (("," ++) . toString) xs) ++ "]"

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var v)   = look v
eval (Oper o e1 e2) = do 
  v1 <- eval e1; 
  v2 <- eval e2; 
  case operate o v1 v2 of
    (Left e)  -> abort (EBadArg e)
    (Right v) -> return v
eval (Not e) = do 
  v <- eval e; 
  if truthy v then return FalseVal else return TrueVal

eval (Call f x) = do
  xs <- eval (List x);
  apply f (extract xs) where
    extract (ListVal x) = x 
    extract _ = [] -- this should never happen

--this is a unnecessarily slowed down bc of the reverse at the end
--this deficiency reocurs  with later ListVal types, 
--specifically in the CCFor evaluation
--there's certainly a way to get rid of this 
--using built in monad functions
eval (List []) = return $ ListVal []
eval (List (e:es)) = evalListAcc (e:es) (ListVal []) where
  --accumulates the value list that is later encapsulated in ListVal
  --upon reaching the end, the list can be "returned" 
  --but the accumulated value is in reverse order
  --the issue was that i couldnt figure out 
  --how to do (x:v) instead of (v:x) --maybe add an empty check
  --since you cant do []:x for example.
  evalListAcc [] (ListVal x) = return (ListVal (reverse x))
  evalListAcc (e:es) (ListVal x) = do
    v <- eval e;
    evalListAcc es (ListVal (v:x))
  evalListAcc _ _ = return NoneVal

--currently only implements the case of with at most one cc
--for concrete implementation ideas see the report
eval (Compr e0 []) = do 
  e <- eval e0;
  return $ ListVal [e] 
eval (Compr e0 ccs) = case head ccs of 
  CCFor x exp -> do
    e <- eval exp; 
    case e of 
      (ListVal []) -> return $ ListVal []
      (ListVal (y:ys)) -> acc (y:ys) (ListVal []) where
        --accumulates the value list that is 
        --later encapsulated in ListVal
        --as mentioned above, z has to be reversed
        --again, probably much better solution possible 
        --with built in monad functions like mapM etc.
        acc [] (ListVal z) = return (ListVal (reverse z))
        acc (y:ys) (ListVal z) = do
          v <- withBinding x y (eval e0);
          acc ys (ListVal (v:z))
        acc _ _ = return NoneVal

      _ -> abort (EBadArg "Expression does not evaluate to a list")

  CCIf exp    -> do
    e <- eval exp;
    if truthy e then do 
      arg <- eval e0
      return (ListVal [arg]) 
    else return $ ListVal []
 

exec :: Program -> Comp ()
exec stmts = if null stmts then output []
  else case head stmts of
    (SDef v exp) -> do
      ev <- eval exp;
      withBinding v ev $ exec $ tail stmts
    (SExp exp) -> do 
      eval exp; 
      exec $ tail stmts;

execute :: Program -> ([String], Maybe RunError)
execute p = case runComp (exec p) [] of 
      (Right _ , out) -> (out, Nothing)
      (Left err, out) -> (out, Just err)  
  
