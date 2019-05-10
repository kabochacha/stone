
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

--token :: Parser String
--token = int <|> ident <|> string
--
--tokens :: Parser [String]
--tokens = many0 (spaces *> token) <* spaces

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
          deriving (Show, Eq)

data Stmt = Simple Expr
          | IfStmt Expr Stmt
          | IfElseStmt Expr Stmt Stmt
          | WhileStmt Expr Stmt
          | List [Stmt]
          deriving (Show, Eq)

zero1 :: a -> Parser a -> Parser a
zero1 a p = p <|> pure a

eol :: Parser String
eol = spaces *> many0 (char ';'<|> char '\n')
--eol :: Parser String
--eol = spaces *> chars ";" <|> many0 (satisfy (\c -> c /= '\n' && isSpace c)) *> (chars "\n")

-------add function-----------

param :: Parser Expr
param = spaces *> (Leaf <$> (IdLit <$> ident))

params :: Parser Expr
params = param <*> many0 (spaces *> char ',' *> param) 

paramList :: Parser Expr
paramList = spaces *> char '(' *> (zero1 () params) <* spaces <* char ')'

def :: Parser
def = spaces *> chars "def" *>
    ((Expr?) <$> param <*> paramList <*> block)

args :: Parser
args = (Expr?) <$> expr <*> many0 (spaces *> char ',' *> expr)

postfix :: Parser
postfix = spaces *> char '(' *> (zero1 () args) <* spaces <* char ')' 

tokenize :: Parser Token
tokenize = spaces *> token
    where token = IntLit  . read <$> int
              <|> BoolLit . read <$> bool
              <|> StrLit         <$> string
              <|> IdLit          <$> ident

primary :: Parser Expr
primary = <$> (spaces *> char '(' *> expr <* spaces <* char ')'
      <|> Leaf <$> tokenize) <*> many0 postfix

--------added function---------

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


block :: Parser Stmt
block = spaces *> char '{' *>
         -- ((\l r -> List (l:r)) <$> (zero1 (List []) statement) <*> many0 (eol *> statement))
        (List <$> ((++) <$> (zero1 [] ((:) <$> statement <*> pure [])) <*> many0 (eol *> statement)))
        <* spaces <* char '}'

statement :: Parser Stmt
statement =  spaces  *> chars    "if" *> (IfElseStmt <$> expr <*> block <*> (spaces *> chars "else" *> block))
         <|> spaces  *> chars    "if" *> (IfStmt     <$> expr <*> block)
         <|> spaces  *> chars "while" *> (WhileStmt  <$> expr <*> block)
         <|> Simple <$> expr

program :: Parser Stmt
program = statement <* eol

