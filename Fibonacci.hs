module Fibonacci (gfib, fib) where

binaryGFib :: (a -> a -> a) -> a -> a -> [a]
binaryGFib f a b = a:binaryGFib f b (f a b)

gfib = binaryGFib

fib :: Integral a => [a]
fib = gfib (+) 0 1
