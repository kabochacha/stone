
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

