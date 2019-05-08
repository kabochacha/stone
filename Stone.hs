
module Stone where

import Parser
import AST
import Eval

sample1 = "a=2\nb = 1\n c  = a+b"
sample2 = "   a =   2; \n   b =       1\n a +b   "
sample3 = "even = 0\nodd = 0\n"
sample4 = "if x == 1 {\n;;a =      6}"
sample5 = "sum  = 0\n   i= 1\n while i <  10 {sum = sum + i\n  i = i+1\n}\n sum;   "
sample6 = "a = True;;;\n b =  False\n a"

test = pull . runParser (many0 program) 
    where pull (Just (x,s)) = x

stone :: String -> Env
stone = snd . eval . test


