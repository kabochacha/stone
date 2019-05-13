
module Stone where

import Parser
import Eval

sample1 = "a=2\nb = 1\n c  = a+b"
sample2 = "   a =   2; \n   b =       1\n a +b   "
sample3 = "even = 0\nodd = 0\n"
sample4 = "if x == 1 {\n;;a =      6}"
sample5 = "sum  = 0\n   i= 1\n while i <  10 {sum = sum + i\n  i = i+1\n}\n sum;   "
sample6 = "a = True;;;\n b =  False\n a"
sample7 = "x = 1\n    def foo(y) {x} \n def bar(x) {foo{x+1}}\n      main = bar(3)"

test = pull . runParser program 
    where
        pull (Just (x,s)) = x
        pull Nothing      = ""

stone :: String -> Env
stone = snd . eval . test


