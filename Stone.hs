
module Stone where

import Parser
import Eval
import Debug.Trace

sample1 = "a=2\nb = 1\n c  = a+b"
sample2 = "   a =   2; \n   b =       1\n a +b   "
sample3 = "even = 0\nodd = 0\n"
sample4 = "if x == 1 {\n;;a =      6}"
sample5 = "sum  = 0\n   i= 1\n while i <  10 {sum = sum + i\n  i = i+1\n}\n sum;   "
sample6 = "a = True;;;\n b =  False\n a"
sample7 = "x = 1\n    def foo(x) {x} \n def bar(x) {foo(x+1)}\n      main = bar(3)"
sample8 = "def foo(x) {  x+ 2 \n  }    \n foo(4)"
sample9 = "y = 1\n    def foo(x) {y + x}\n    def bar(x) { y = x+ 10\n  foo(x) }\n         bar(1)"
sample10 = "def foo(x) {x+1}\n     def bar(x) {  foo(x+10)  } \n      bar(5)"
sample11 = " q = 199 a = [1,32,9,q]  def foo(x) {a[x]}\n     def bar(x) {  foo(x+1)  } \n      bar(2)"

test = pull . runParser program 
    where
        pull (Just (x,s)) = x
        pull Nothing      = []

stone' :: String -> IO [Result]
stone' s = eval $ test s

stone :: String -> IO Result
stone s = do
    r <- eval $ test s
    return $ last r




