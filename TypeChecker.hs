
module TypeChecker where

import Parser
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import Data.IORef
import Debug.Trace
import System.IO.Unsafe

data Env = Env {localEnv  :: IORef (Map.Map String Type),
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

data Type = Any | Int | Boolean | String | FuncType [Type] Type | ArrayType Type | TypeError String
    deriving (Eq, Show)

isSubType :: Type -> Type -> Bool
isSubType Int     Int     = True
isSubType Boolean Boolean = True
isSubType String  String  = True
isSubType (ArrayType t1) (ArrayType t2) = isSubType t1 t2
isSubType (FuncType ts1 t1) (FuncType ts2 t2) = all (==True) (zipWith isSubType ts2 ts1) && isSubType t1 t2
isSubType (TypeError _) _ = False
isSubType _ (TypeError _) = False
isSubType _ Any = True
isSubType Any _ = False
isSubType _ _ = False

typeCheckBin1 :: Op1 -> Env -> IO Type
typeCheckBin1 (Add e1 e2) env = do
    t1 <- typeCheckExpr e1 env
    t2 <- typeCheckExpr e2 env
    case (t1,t2) of
      (Int,Int)       -> return Int
      (String,String) -> return String
      (_,_)           -> return $ TypeError ("cannot " ++ show t1 ++ " \'+\' " ++ show t2 ++ "!")
typeCheckBin1 (Sub e1 e2) env = do
    t1 <- typeCheckExpr e1 env
    t2 <- typeCheckExpr e2 env
    case (t1,t2) of
      (Int,Int) -> return Int
      (_,_)     -> return $ TypeError ("cannot " ++ show t1 ++ " \'-\' " ++ show t2 ++ "!")
typeCheckBin1 (Mul e1 e2) env = do
    t1 <- typeCheckExpr e1 env
    t2 <- typeCheckExpr e2 env
    case (t1,t2) of
      (Int,Int) -> return Int
      (_,_)     -> return $ TypeError ("cannot " ++ show t1 ++ " \'*\' " ++ show t2 ++ "!")
typeCheckBin1 (Div e1 e2) env = do
    t1 <- typeCheckExpr e1 env
    t2 <- typeCheckExpr e2 env
    case (t1,t2) of
      (Int,Int) -> return Int
      (_,_)     -> return $ TypeError ("cannot " ++ show t1 ++ " \'/\' " ++ show t2 ++ "!")
typeCheckBin1 (Equ e1 e2) env = return Boolean
typeCheckBin1 (Mor e1 e2) env = do
    t1 <- typeCheckExpr e1 env
    t2 <- typeCheckExpr e2 env
    case (t1,t2) of
      (Int,Int) -> return Boolean
      (_,_)     -> return $ TypeError ("cannot compare " ++ show t1 ++ " with " ++ show t2 ++ "!")
typeCheckBin1 (Les e1 e2) env = do
    t1 <- typeCheckExpr e1 env
    t2 <- typeCheckExpr e2 env
    case (t1,t2) of
      (Int,Int) -> return Boolean
      (_,_)     -> return $ TypeError ("cannot compare " ++ show t1 ++ " with " ++ show t2 ++ "!")

typeCheckBin0 :: Op0 -> Env -> IO Type
typeCheckBin0 op0@(Ass (Leaf (IdLit p)) e) env = do
    t <- typeCheckExpr e env
    local <- readIORef $ localEnv env
    case Map.lookup p local of
      Nothing -> case parentEnv env of
                   Nothing -> do
                       modifyIORef (localEnv env) (Map.insert p Any) -- FIXME
                       return Any
                   Just global -> typeCheckBin0 op0 global
      Just t0 -> if not (t `isSubType` t0) then
                 return (TypeError ("cannot assign " ++ show t0 ++ " to " ++ show t ++ "!")) else do
                     --modifyIORef (localEnv env) (Map.insert p t0)
                     return t0
typeCheckBin0 _ _ = return $ TypeError "not an assignable variable!"

typeCheckFuncCall :: Type -> [Type] -> Env -> IO Type
typeCheckFuncCall (FuncType arguments tf) vs env =
    if length arguments /= length vs then return (TypeError "not exact number of arguments!")
    else if all (==True) (zipWith isSubType vs arguments) then return tf else
    return $ TypeError "variable types not matching argument types!"
typeCheckFuncCall (ArrayType t) ts env =
    if length ts > 1 || length ts == 0 then return (TypeError "zero index or too many indices!") else do
        if (ts!!0) == Int  || (ts!!0) == Any then return t
        else return (TypeError (show t ++ " type cannot be a Array index!"))
typeCheckFuncCall _ _ _ = return $ TypeError "not a callable variable!"

typeCheckExpr :: Expr -> Env -> IO Type
typeCheckExpr (Leaf (IntLit  i)) _   = return Int
typeCheckExpr (Leaf (BoolLit b)) _   = return Boolean
typeCheckExpr (Leaf (StrLit  s)) _   = return String
typeCheckExpr e@(Leaf (IdLit p)) env = do
    local <- readIORef $ localEnv env
    case Map.lookup p local of
      Just t  -> return t
      Nothing -> case parentEnv env of
                   Nothing     -> return $ TypeError "No such variable!"
                   Just global -> typeCheckExpr e global
typeCheckExpr (NegExpr e) env = do
    t <- typeCheckExpr e env
    case t of
      Int -> return Int
      _   -> return $ TypeError ("cannot negate type " ++ show t ++ "!")
typeCheckExpr (BinaryExpr0 op0) env = typeCheckBin0 op0 env
typeCheckExpr (BinaryExpr1 op1) env = typeCheckBin1 op1 env
typeCheckExpr (FuncCall f arguments) env = do
    ts <- forM [0..((length arguments)-1)] $ \i -> typeCheckExpr (arguments!!i) env
    tf <- typeCheckExpr f env
    typeCheckFuncCall tf ts env
typeCheckExpr (Array []) env = return $ ArrayType Any --FIXME
typeCheckExpr (Array es) env = do
    ts <- forM [0..((length es)-1)] $ \i -> typeCheckExpr (es!!i) env
    if (allEqual ts) then return (ArrayType (head ts))
    else return $ TypeError "multiple types in a single list!"
typeCheckExpr (TypeTag _ [])          env = return Any
typeCheckExpr (TypeTag _ [(Leaf (IdLit s))]) env = return $ string2Type s

typeCheckStmt :: Stmt -> Env -> IO Type
typeCheckStmt (Simple e) env = typeCheckExpr e env 
typeCheckStmt (IfStmt e s) env = do
    t <- typeCheckExpr e env
    if t /= Boolean then return (TypeError (show t ++ " type cannot be in the IF statement"))
    else typeCheckStmt s env
typeCheckStmt (IfElseStmt e s1 s2) env = do
    t <- typeCheckExpr e env
    if t /= Boolean then return (TypeError (show t ++ " type cannot be in the IF statement"))
    else typeCheckStmt s1 env --FIXME
typeCheckStmt (WhileStmt e s) env = do
    t <- typeCheckExpr e env
    if t /= Boolean then return (TypeError (show t ++ " type cannot be in the WHILE statement"))
    else typeCheckStmt s env
typeCheckStmt (List [])  _   = return Any
typeCheckStmt (List [s]) env = typeCheckStmt s env
typeCheckStmt (List (s:ss)) env = do
    typeCheckStmt (List ss) env
typeCheckStmt (Def (Leaf (IdLit p)) arguments tag stmt) env = do
    ts <- forM [0..((length arguments)-1)] $ \i -> typeCheckExpr (arguments!!i) env
    case tag of
      [] -> do
          modifyIORef (localEnv env) (Map.insert p (FuncType ts Any))
          return $ FuncType ts Any
      [(Leaf (IdLit s))] -> let t0 = string2Type s in
          if t0 == (TypeError "unknown type!") then return t0 else do
              modifyIORef (localEnv env) (Map.insert p (FuncType ts t0))
              return $ FuncType ts t0
typeCheckStmt (Var (Leaf (IdLit p)) [] _) env = do
    modifyIORef (localEnv env) (Map.insert p Any)
    return Any
typeCheckStmt (Var (Leaf (IdLit p)) [(Leaf (IdLit s))] e) env =
    let t0 = string2Type s in do
        t1 <- typeCheckExpr e env
        if t0 == (TypeError "unknown type!") then return t0
        else if t1 == Any || (t1 `isSubType` t0) then do
            modifyIORef (localEnv env) (Map.insert p t0)
            return t0
        else return $ TypeError ("declared type " ++ show t0 ++ " not matching actual type " ++ show t1 ++ "!")

string2Type :: String -> Type
string2Type "Int" = Int
string2Type "Bool" = Boolean
string2Type "String" = String
string2Type ('L':'i':'s':'t':s') = ArrayType $ string2Type s'
string2Type _ = TypeError "unknown type!"

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:y:ys) = (x==y) && allEqual (y:ys)

typeCheck :: [Stmt] -> IO [Type]
typeCheck stmts = do
    global <- newIORef Map.empty :: IO (IORef (Map.Map String Type))
    sequence $ map (`typeCheckStmt` (Env global Nothing)) stmts


