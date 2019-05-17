
module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

--------------Basics--------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
    fmap g (Parser f) = Parser (fmap (fmap (first g)) f)

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x,s)
    p1 <*> p2 = Parser f where
        f s = case runParser p1 s of 
            Nothing -> Nothing
            Just (g,xs) -> case runParser p2 xs of 
                Nothing -> Nothing
                Just (h,ys) -> Just (g h, ys)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser f where
        f s = runParser p1 s <|> runParser p2 s

instance Monad Parser where
    return x = Parser f
        where f s = Just (x, s)
    p >>= f = Parser $ \s -> do
        (a, xs) <- runParser p s
        (b, ys) <- runParser (f a) xs
        return (b, ys)

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many0 p

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
        f [] = Nothing
        f (c:cs)
            | p c       = Just (c,cs)
            | otherwise = Nothing

-------------Unit Parsers---------------

char :: Char -> Parser Char
char c = satisfy (==c)

chars :: String -> Parser String
chars     [] = pure []
chars (c:cs) = (:) <$> char c <*> chars cs

digit :: Parser Char
digit = satisfy isDigit

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

spaces :: Parser String
spaces = many0 (satisfy isSpace)

int :: Parser String
int = many1 digit

bool :: Parser String
bool = chars "True" <|> chars "False"

string :: Parser String
string = char '\"' *> many1 alpha <* char '\"'

ident :: Parser String
ident = (:) <$> alpha <*> many0 alphaNum

------------AST----------------

data Token = IntLit  Integer
           | BoolLit Bool
           | StrLit  String
           | IdLit   String
           deriving (Show, Eq)
     
data Op1 = Add Expr Expr
         | Sub Expr Expr
         | Mul Expr Expr
         | Div Expr Expr
         | Mod Expr Expr
         | Equ Expr Expr
         | Les Expr Expr
         | Mor Expr Expr
        deriving (Show, Eq)

data Op0 = Ass Expr Expr
    deriving (Show, Eq)

data Expr = Leaf Token
          | NegExpr Expr
          | BinaryExpr0 Op0
          | BinaryExpr1 Op1
          | FuncCall Expr [Expr]
          | Array [Expr]
          deriving (Eq)

instance Show Expr where
    show (Leaf t) = showsPrec 11 t ""
    show (NegExpr e) = "-" ++ showsPrec 10 e ""
    show (FuncCall e1 es) = "(" ++ "fc"  ++ showsPrec 10 e1 "" ++ show es ++ ")"
    show (Array es) = show es
    show (BinaryExpr0 (Ass e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " = "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Add e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " + "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Sub e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " - "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Mul e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " * "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Div e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " / "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Mod e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " % "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Equ e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " == " ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Mor e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " > "  ++ showsPrec 10 e2 "" ++ ")"
    show (BinaryExpr1 (Les e1 e2)) = "(" ++ showsPrec 10 e1 "" ++ " < "  ++ showsPrec 10 e2 "" ++ ")"

data Stmt = Simple Expr 
          | IfStmt Expr Stmt
          | IfElseStmt Expr Stmt Stmt
          | WhileStmt Expr Stmt
          | List [Stmt]
          | Def Expr [Expr] Stmt
          deriving (Show, Eq)

zero1 :: a -> Parser a -> Parser a
zero1 a p = p <|> pure a

eol :: Parser String
eol = spaces *> many0 (char ';'<|> char '\n')

numberExpr :: Parser Expr
numberExpr = spaces *> (Leaf <$> (IntLit . read <$> int))

idExpr :: Parser Expr
idExpr = spaces *> (Leaf <$> (IdLit <$> ident))

boolExpr :: Parser Expr
boolExpr = spaces *> (Leaf <$> (BoolLit . read <$> bool))

stringExpr :: Parser Expr
stringExpr = spaces *> (Leaf <$> (StrLit <$> string))

param :: Parser Expr
param = idExpr

params :: Parser [Expr]
params = (:) <$> param <*> many0 (spaces *> char ',' *> param)

paramList :: Parser [Expr]
paramList = spaces *> char '(' *>
    (zero1 [] params)
    <* spaces <* char ')'

args :: Parser [Expr]
args = (:) <$> expr <*> many0 (spaces *> char ',' *> expr)

postfix :: Parser [Expr]
postfix = spaces *> char '(' *> (zero1 [] args)        <* spaces <* char ')' 
      <|> spaces *> char '[' *> ((\e -> [e]) <$> expr) <* spaces <* char ']'

primary2 :: Parser Expr
primary2 = spaces *> char '[' *> (Array <$> elements) <* spaces <* char ']'
       <|> spaces *> char '(' *> expr                 <* spaces <* char ')'
       <|> numberExpr
       <|> boolExpr
       <|> stringExpr
       <|> idExpr

primary :: Parser Expr
primary = foldl (flip ($)) <$> primary2 <*> many0 (flip FuncCall <$> postfix) -- not many1?
      <|> primary2

factor :: Parser Expr
factor = spaces *> (NegExpr <$> (char '-' *> primary) <|> primary)

op0 :: Parser (Expr -> Expr -> Op0)
op0 = spaces *> (Ass <$ char '=')

op1 :: Parser (Expr -> Expr -> Op1)
op1    = spaces *>
       (Add <$ char  '+'
    <|> Sub <$ char  '-'
    <|> Mul <$ char  '*'
    <|> Div <$ char  '/'
    <|> Mod <$ char  '%'
    <|> Equ <$ chars "=="
    <|> Les <$ char  '<'
    <|> Mor <$ char  '>')

expr  :: Parser Expr
expr  = spaces *> (flip (foldr ($)) <$> many0 ((\l op r -> BinaryExpr0 (op l r)) <$> expr1 <*> op0) <*> expr1)

expr1 :: Parser Expr
expr1 = spaces *> (foldl (flip ($)) <$> factor <*> many0 ((\op' r l -> BinaryExpr1 (op' l r)) <$> op1 <*> factor))

elements :: Parser [Expr]
elements = (:) <$> expr <*> many0 (char ',' *> expr)

simple :: Parser Stmt
simple = Simple <$> expr

def :: Parser Stmt
def = spaces *> chars "def" *>
    (Def <$> idExpr <*> paramList <*> block)

block :: Parser Stmt
block = spaces *> char '{' *>
        (List <$> ((++) <$> (zero1 [] ((:) <$> statement <*> pure [])) <*> many0 (eol *> statement)))
        <* spaces <* char '}'

statement :: Parser Stmt
statement =  spaces  *> chars    "if" *> (IfElseStmt <$> expr <*> block <*> (spaces *> chars "else" *> block))
         <|> spaces  *> chars    "if" *> (IfStmt     <$> expr <*> block)
         <|> spaces  *> chars "while" *> (WhileStmt  <$> expr <*> block)
         <|> simple

program :: Parser [Stmt]
program = many0 ((def <|> statement) <* eol)




