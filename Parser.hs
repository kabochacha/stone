
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

---------AST----------

data Token a = IntLit  a
             | BoolLit a
             | StrLit  a
             | IdLit   a
             | Shit
             deriving (Show, Eq)
     
data Op = Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Mod Expr Expr
        | Equ Expr Expr
        | Les Expr Expr
        | Mor Expr Expr
        | Ass Expr Expr
        deriving (Show, Eq)

data Expr = Leaf (Token String)
          | NegExpr Expr
          | BinaryExpr Op
          deriving (Show, Eq)

data State = Simple Expr
           | IfState Expr State
           | IfElseState Expr State State
           | WhileState Expr State
           | List [State]
           deriving (Show, Eq)

zero1 :: a -> Parser a -> Parser a
zero1 a p = p <|> pure a

eol :: Parser String
eol = spaces *> many0 (char ';' <|> char '\n')
--eol :: Parser Char
--eol = spaces *> char ';' <|> many0 (satisfy (\c -> c /= '\n' && isSpace c)) *> (char '\n')

tokenize :: Parser (Token String)
tokenize = spaces *> token
    where token = IntLit  <$> int
              <|> BoolLit <$> bool
              <|> StrLit  <$> string
              <|> IdLit   <$> ident

primary :: Parser Expr
primary = spaces *> char '(' *> expr <* spaces <* char ')'
      <|> Leaf <$> tokenize

factor :: Parser Expr
factor = spaces *> (NegExpr <$> (char '-' *> primary) <|> primary)

op :: Parser (Expr -> Expr -> Op)
op    = spaces *> (Add <$ char '+'
    <|> Sub <$ char  '-'
    <|> Mul <$ char  '*'
    <|> Div <$ char  '/'
    <|> Mod <$ char  '%'
    <|> Equ <$ chars "=="
    <|> Les <$ char  '<'
    <|> Mor <$ char  '>'
    <|> Ass <$ char  '=')

expr :: Parser Expr
expr = spaces *> (foldl (flip ($)) <$> factor <*> many0 ((\op' l r -> BinaryExpr (op' r l)) <$> op <*> factor))

block :: Parser State
block = spaces *> char '{' *>
         -- ((\l r -> List (l:r)) <$> (zero1 (List []) statement) <*> many0 (eol *> statement))
        (List <$> ((++) <$> (zero1 [] ((:) <$> statement <*> pure [])) <*> many0 (eol *> statement)))
        <* spaces <* char '}'

statement :: Parser State
statement = spaces  *> chars    "if" *> (IfState     <$> expr <*> block)
        <|> spaces  *> chars    "if" *> (IfElseState <$> expr <*> block <*> (spaces *> chars "else" *> block))
        <|> spaces  *> chars "while" *> (WhileState  <$> expr <*> block)
        <|> Simple <$> expr

program :: Parser State
program = statement <* eol

--------Eval---------

instance Functor Token where
    fmap f (IntLit  a) = IntLit  $ f a
    fmap f (BoolLit a) = BoolLit $ f a
    fmap f (StrLit  a) = StrLit  $ f a
    fmap f (IdLit   a) = IdLit   $ f a
    fmap _ Shit        = Shit

instance Applicative Token where
    pure = IntLit --FIXME
    IntLit  f <*> IntLit  a = fmap f $ IntLit  a
    BoolLit f <*> BoolLit a = fmap f $ BoolLit a
    StrLit  f <*> StrLit  a = fmap f $ StrLit  a
    IdLit   f <*> IdLit   a = fmap f $ IdLit   a
    _         <*> _         = Shit

--using hash??

assign :: (Token String) -> (Token String) -> (Token String) --FIXME
assign = undefined

binary :: (Int -> Int -> Int) -> String -> String -> String
binary f s1 s2 = show $ f (read s1 :: Int) (read s2 :: Int) 

evalBinary :: Op -> Token String
evalBinary op = case op of
                         Add e1 e2 -> binary (+) <$> evalExpr e1 <*> evalExpr e2
                         Sub e1 e2 -> binary (-) <$> evalExpr e1 <*> evalExpr e2
                         Mul e1 e2 -> binary (*) <$> evalExpr e1 <*> evalExpr e2
                         Div e1 e2 -> binary div <$> evalExpr e1 <*> evalExpr e2
                         Mod e1 e2 -> binary mod <$> evalExpr e1 <*> evalExpr e2
                         Equ e1 e2 -> binary (==)       <$> evalExpr e1 <*> evalExpr e2
                         Les e1 e2 -> binary (<)        <$> evalExpr e1 <*> evalExpr e2
                         Mor e1 e2 -> binary (>)        <$> evalExpr e1 <*> evalExpr e2
                         Ass e1 e2 -> assign (evalExpr e1) (evalExpr e2) 

--evalExpr :: Expr -> Result
--evalExpr (Leaf t) = toRes t
--evalExpr (NegExpr e) = (-1) * evalExpr e
--evalExpr (Binary op) = evalBinary op
--
--evalState :: State -> [Result]
--evalState  (Simple e) = evalExpr e
--evalState  (IfState e s) = if evalExpr e then evalState s else []
--evalState  (IfElseState e s1 s2) = if evalExpr e then evalState s1 else evalState s2
--evalState  (WhileState e s) = if evalExpr e then evalState s else []
--evalState  (List ss) = case ss of
--                   [] -> []
--                   x:xs -> evalState x : evalState xs

evalExpr :: Expr -> Token String
evalExpr = undefined
--evalExpr (Leaf t) = t
--evalExpr (NegExpr e) = do
--    num <- evalExpr e
--    (-1) * num
--evalExpr (Binary op) = evalBinary op
--
--evalState :: State -> IO ()
--evalState (Simple e) = do
--    evalExpr e
--    return ()
--evalState (IfState e s) = do
--    if evalExpr e == (BoolLit "True") then
--                  evalState s
--                  else return ()
--evalState (IfElseState e s1 s2) = do
--    if evalExpr e == (BoolLot "True") then
--                  evalState s1
--                  else evalState s2
--evalState (WhileState e s) = do
--    if evalExpr e == (BoolLit "True") then
--                  evalState s
--                  evalState (WhileState e s)
--                  else return ()
--evalState (List ss) = case ss of
--                   [] -> return ()
--                   x:xs -> do
--                       evalState x
--                       evalState xs

------------------



sample1 = "a=2\nb = 1\na+b"
sample2 = "   a =   2; \n   b =       1\n a +b   "
sample3 = "even = 0\nodd = 0\n"
sample4 = "if x == 1 {\n;;a =      6}"
sample5 = "sum  = 0\n   i= 1\n while i <  10 {sum = sum + i\n  i = i+1\n}\n sum;   "
sample6 = "a = True;;;\n b =  False\n a"



