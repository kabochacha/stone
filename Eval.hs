
module Eval where

import Parser
import AST
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import qualified Control.Monad.State as St

newtype Env = Env {runEnv :: Map.Map String Result}
    deriving (Eq)

instance Show Env where
    show (Env dic) = f (Map.toList dic) where
        f [] = ""
        f (x:xs) = (fst x) ++ " = " ++ show (snd x) ++ "\n" ++ f xs

data Result = I Integer | B Bool | S String | Shit
    deriving (Eq, Show)

intBinary :: (Integer -> Integer -> Integer) -> Expr -> Expr -> St.State Env (Result)
intBinary f e1 e2 = do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return (I ((\(I i1) (I i2) -> f i1 i2) r1 r2))

evalBinary :: Op -> St.State Env (Result)
evalBinary (Add e1 e2) = intBinary (+) e1 e2
evalBinary (Sub e1 e2) = intBinary (-) e1 e2
evalBinary (Mul e1 e2) = intBinary (*) e1 e2
evalBinary (Div e1 e2) = intBinary div e1 e2
evalBinary (Mod e1 e2) = intBinary mod e1 e2
evalBinary (Equ e1 e2) = do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return (B (r1==r2))
evalBinary (Mor e1 e2) = do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return $ B ((\(I i1) (I i2) -> i1 > i2) r1 r2)
evalBinary (Les e1 e2) = do
    r1 <- evalExpr e1
    r2 <- evalExpr e2
    return $ B ((\(I i1) (I i2) -> i1 < i2) r1 r2)
evalBinary (Ass (Leaf (IdLit p)) e) = do
    v <- evalExpr e
    env <- St.get
    St.put . Env $ Map.insert p v (runEnv env)
    return v

evalExpr :: Expr -> St.State Env (Result)
evalExpr (Leaf (IntLit  i)) = return (I i)
evalExpr (Leaf (BoolLit b)) = return (B b)
evalExpr (Leaf (StrLit  s)) = return (S s)
evalExpr (Leaf (IdLit   p)) = do
    env <- St.get
    case Map.lookup p (runEnv env) of
      Nothing -> return (Shit) -- error
      Just r  -> return r
evalExpr (NegExpr e) = do
    r <- evalExpr e
    return $ I ((\(I i) -> (-1) * i) r)
evalExpr (BinaryExpr op) = evalBinary op

evalStmt :: Stmt -> St.State Env ()
evalStmt (Simple e) = do
    evalExpr e
    return ()
evalStmt (IfStmt e s) = do
    r <- evalExpr e
    if r == (B True) then
         evalStmt s
    else
         return ()
evalStmt (IfElseStmt e s1 s2) = do
    r <- evalExpr e
    if r == (B True) then
         evalStmt s1
    else
         evalStmt s2
evalStmt p@(WhileStmt e s) = do
    r <- evalExpr e
    if r == (B True) then do
         evalStmt s
         evalStmt p
    else
         return ()
evalStmt (List [])   = return ()
evalStmt (List (s:ss)) = do
    evalStmt s
    evalStmt $ List ss

eval :: [Stmt] -> ((),Env)
eval stmts = St.runState (sequence_ (map evalStmt stmts)) (Env Map.empty)



