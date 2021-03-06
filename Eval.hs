
module Eval where

import Parser
import TypeChecker hiding(Env, localEnv, parentEnv, showEnv, printEnv)
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import Data.IORef
import Debug.Trace
import System.IO.Unsafe

data Env = Env {localEnv  :: IORef (Map.Map String Result),
                parentEnv :: Maybe Env} deriving (Eq)

showEnv :: Env -> IO String
showEnv env = do
    local <- readIORef $ localEnv env
    case parentEnv env of
      Nothing -> do
        return $ "Env " ++ showsPrec 11 local "" ++ " Nothing"
      Just p -> do
        pp <- showEnv p
        return $ "Env " ++ showsPrec 11 local "" ++ " (" ++ pp ++ ")" 

printEnv :: Env -> IO ()
printEnv env = showEnv env >>= putStrLn

data Result = Arr [Result] | Func Stmt Env | I Integer | B Bool | S String | NoResult | Shit String
    deriving (Eq)

instance Show Result where
    show (Arr rs)   = show rs
    show (Func s _) = show s
    show (I i)      = show i
    show (B b)      = show b
    show (S s)      = s
    show NoResult   = ""
    show (Shit s)   = s

intBinary :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Env -> IO Result
intBinary f e1 e2 env = do
    r1 <- evalExpr e1 env
    r2 <- evalExpr e2 env
    return $ f' f r1 r2
        where
            f' f (I i1) (I i2) = I $ f i1 i2

evalBinary1 :: Op1 -> Env -> IO Result
evalBinary1 (Add e1 e2) env = do
    r1 <- evalExpr e1 env
    r2 <- evalExpr e2 env
    case (r1,r2) of
      (I i1, I i2) -> return $ I (i1+i2)
      (S s1, S s2) -> return $ S (s1++s2)
evalBinary1 (Sub e1 e2) env = intBinary (-) e1 e2 env
evalBinary1 (Mul e1 e2) env = intBinary (*) e1 e2 env
evalBinary1 (Div e1 e2) env = intBinary div e1 e2 env
evalBinary1 (Mod e1 e2) env = intBinary mod e1 e2 env
evalBinary1 (Equ e1 e2) env = do
    r1 <- evalExpr e1 env
    r2 <- evalExpr e2 env
    return $ B (r1==r2)
evalBinary1 (Mor e1 e2) env = do
    r1 <- evalExpr e1 env
    r2 <- evalExpr e2 env
    return $ B (f' r1 r2)
        where
            f' (I i1) (I i2) = i1 > i2
evalBinary1 (Les e1 e2) env = do
    r1 <- evalExpr e1 env
    r2 <- evalExpr e2 env
    return $ B (f' r1 r2)
        where
            f' (I i1) (I i2) = i1 < i2

evalBinary0 :: Op0 -> Env -> IO Result
evalBinary0 (Ass (Leaf (IdLit p)) e) env = do
    v <- evalExpr e env
    modifyIORef (localEnv env) (Map.insert p v)
    return v

getName :: Expr -> IO String
getName (Leaf (IdLit p)) = return p
getName (TypeTag (Leaf (IdLit p)) _) = return p
getName _                = return "" --FIXME

evalFunc :: Result -> [Expr] -> Env -> IO Result
evalFunc (Func s@(Def name arguments _ stmt) defEnv) things evalEnv = do
    local <- newIORef (Map.empty) :: IO (IORef (Map.Map String Result))
    a <- forM [0..((length arguments)-1)] $ \i -> do
        k <- getName (arguments!!i) -- what if not a (S s)?
        v <- evalExpr (things!!i) evalEnv
        --modifyIORef (localEnv (Env local (Just defEnv))) (Map.insert k v)
        modifyIORef local (Map.insert k v)
    -- ll <- readIORef local
    -- trace ("call " ++ show name ++ " with " ++ show ll) (return ())
    evalStmt stmt (Env local (Just defEnv))
evalFunc (Arr rs) e env = do
    r <- evalExpr (e!!0) env
    return (f r) where
        f (I i) = (rs!!index) where index = fromInteger i

evalExpr :: Expr -> Env -> IO Result
evalExpr (Leaf (IntLit  i)) _   = return $ I i
evalExpr (Leaf (BoolLit b)) _   = return $ B b
evalExpr (Leaf (StrLit  s)) _   = return $ S s
evalExpr e@(Leaf (IdLit p)) env = do
    -- trace ("read " ++ p ++ " in " ++ unsafePerformIO (showEnv env)) (return ())
    local <- readIORef $ localEnv env
    case Map.lookup p local of
      -- Just r  -> trace ("--> " ++ show r) $ return r
      Just r  -> return r
      Nothing -> case parentEnv env of
                   Nothing     -> return $ Shit "no such value!"
                   Just global -> evalExpr e global
evalExpr (NegExpr e) env = do
    r <- evalExpr e env
    return $ f r where
        f (I i) = I $ (-1) * i
evalExpr (BinaryExpr0 op0) env = evalBinary0 op0 env
evalExpr (BinaryExpr1 op1) env = evalBinary1 op1 env
evalExpr (FuncCall f arguments) env = do
    func <- evalExpr f env
    evalFunc func arguments env
-- evalExpr (Array []) env = return $ Arr []
evalExpr (Array es) env = do
    a <- forM [0..((length es) - 1)] $ \i -> evalExpr (es!!i) env
    return $ Arr a
evalExpr (TypeTag e _) env = evalExpr e env

evalStmt :: Stmt -> Env -> IO Result
evalStmt (Simple e) env = evalExpr e env
evalStmt (IfStmt e s) env = do
    r <- evalExpr e env
    if r == (B True) then
        evalStmt s env
    else
        return NoResult
evalStmt (IfElseStmt e s1 s2) env = do
    r <- evalExpr e env
    if r == (B True) then
        evalStmt s1 env
    else
        evalStmt s2 env
evalStmt p@(WhileStmt e s) env = do
    r <- evalExpr e env
    if r == (B True) then do
        evalStmt s env
        evalStmt p env
    else
        return NoResult --FIXME
evalStmt (List [])     _   = return NoResult
evalStmt (List [s])    env = evalStmt s env
evalStmt (List (s:ss)) env = do
    evalStmt s env
    evalStmt (List ss) env
evalStmt s@(Def (Leaf (IdLit p)) arguments _ stmt) env = do
    modifyIORef (localEnv env) (Map.insert p (Func s env))
    return NoResult
evalStmt (Var (Leaf (IdLit p)) _ e) env = do
    r <- evalExpr e env
    modifyIORef (localEnv env) (Map.insert p r)
    return r


eval :: [Stmt] -> IO [Result]
eval stmts = do
    global <- newIORef Map.empty :: IO (IORef (Map.Map String Result))
    sequence $ map (`evalStmt` (Env global Nothing)) stmts


