
module Eval where

import Parser
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
            f' _ _      _      = Shit "not a binary-operatable variable!"

evalBinary1 :: Op1 -> Env -> IO Result
evalBinary1 (Add e1 e2) env = intBinary (+) e1 e2 env
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
            f' _      _      = False
evalBinary1 (Les e1 e2) env = do
    r1 <- evalExpr e1 env
    r2 <- evalExpr e2 env
    return $ B (f' r1 r2)
        where
            f' (I i1) (I i2) = i1 < i2
            f' _      _      = False

evalBinary0 :: Op0 -> Env -> IO Result
evalBinary0 (Ass (Leaf (IdLit p)) e) env = do
    v <- evalExpr e env
    modifyIORef (localEnv env) (Map.insert p v)
    return v
evalBinary0 _ _ = return $ Shit "not a assignable variable!"

getName :: Expr -> IO String
getName (Leaf (IdLit p)) = return p
getName _                = return "" --FIXME

evalFunc :: Result -> [Expr] -> Env -> IO Result
evalFunc (Func s@(Def name args stmt) defEnv) things evalEnv =
    if length args /= length things then return (Shit "not exact number of parameters!") else do
        local <- newIORef (Map.empty) :: IO (IORef (Map.Map String Result))
        a <- forM [0..((length args)-1)] $ \i -> do
            k <- getName (args!!i) -- what if not a (S s)?
            v <- evalExpr (things!!i) evalEnv
            --modifyIORef (localEnv (Env local (Just defEnv))) (Map.insert k v)
            modifyIORef local (Map.insert k v)
        -- ll <- readIORef local
        -- trace ("call " ++ show name ++ " with " ++ show ll) (return ())
        evalStmt stmt (Env local (Just defEnv)) -- which one should be the parentEnv?
evalFunc (Arr rs) e env =
    if (length e) > 1 || (length e) == 0 then return (Shit "zero index or too many indices!") else do
        r <- evalExpr (e!!0) env
        return (f r) where
            f (I i)
              | index >= (length rs) || index < 0 = Shit "index out of range!"
              | otherwise = (rs!!index)
                where
                    index = fromInteger i
            f _ = Shit "not a proper index!"
evalFunc _ _ _ = return $ Shit "not a callable value!"

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
        f _     = Shit "not a negtive number!"
evalExpr (BinaryExpr0 op0) env = evalBinary0 op0 env
evalExpr (BinaryExpr1 op1) env = evalBinary1 op1 env
evalExpr (FuncCall f args) env = do
    func <- evalExpr f env
    evalFunc func args env
-- evalExpr (Array []) env = return $ Arr []
evalExpr (Array es) env = do
    a <- forM [0..((length es) - 1)] $ \i ->
        evalExpr (es!!i) env
    return $ Arr a

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
        m <- evalStmt s env
        n <- evalStmt p env
        return n
    else
        return NoResult --FIXME
evalStmt (List [])     _   = return NoResult
evalStmt (List [s])    env = evalStmt s env
evalStmt (List (s:ss)) env = do
    evalStmt s env
    evalStmt (List ss) env
evalStmt s@(Def e es stmt) env = do
    k <- getName e
    modifyIORef (localEnv env) (Map.insert k (Func s env))
    return NoResult

eval :: [Stmt] -> IO [Result]
eval stmts = do
    global <- newIORef Map.empty :: IO (IORef (Map.Map String Result))
    sequence $ map (`evalStmt` (Env global Nothing)) stmts


