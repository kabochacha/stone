
module AST where

import Control.Applicative
import Control.Monad
import Data.Char
import Parser

data Token = IntLit  Integer
           | BoolLit Bool
           | StrLit  String
           | IdLit   String
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

data Expr = Leaf Token
          | NegExpr Expr
          | BinaryExpr Op
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
eol = spaces *> many0 (char ';' <|> char '\n')
--eol :: Parser Char
--eol = spaces *> char ';' <|> many0 (satisfy (\c -> c /= '\n' && isSpace c)) *> (char '\n')

tokenize :: Parser Token
tokenize = spaces *> token
    where token = IntLit  . read <$> int
              <|> BoolLit . read <$> bool
              <|> StrLit         <$> string
              <|> IdLit          <$> ident

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

block :: Parser Stmt
block = spaces *> char '{' *>
         -- ((\l r -> List (l:r)) <$> (zero1 (List []) statement) <*> many0 (eol *> statement))
        (List <$> ((++) <$> (zero1 [] ((:) <$> statement <*> pure [])) <*> many0 (eol *> statement)))
        <* spaces <* char '}'

statement :: Parser Stmt
statement = spaces  *> chars    "if" *> (IfStmt     <$> expr <*> block)
        <|> spaces  *> chars    "if" *> (IfElseStmt <$> expr <*> block <*> (spaces *> chars "else" *> block))
        <|> spaces  *> chars "while" *> (WhileStmt  <$> expr <*> block)
        <|> Simple <$> expr

program :: Parser Stmt
program = statement <* eol

