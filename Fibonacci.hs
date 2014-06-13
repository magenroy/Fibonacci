module Fibonacci (gfib, fib) where

binaryGFib :: (a -> a -> a) -> a -> a -> [a]
binaryGFib f a b = a:binaryGFib f b (f a b)

itaiGFib :: ([a] -> a) -> [a] -> [a]
itaiGFib f init@(x:xs) = x:itaiGFib f (xs ++ [f init])

gFib :: ([a] -> [a]) -> [a] -> [a]
gFib f init = init ++ (drop (length init) $ gFib f $ init ++ (f init))
gFib' f init = let next f' init' = f' init' ++ (next f' $ init' ++ (f' init')) in init ++ (next f init)

gfib = binaryGFib

fib :: Integral a => [a]
fib = gfib (+) 0 1

nfib n = gFib (\x -> [sum $ drop (length x - n) x]) [1]
tst n = let fbl = tail $ nfib n in zipWith (-) [ 2^x | x <- [0..]] fbl
tst' n = [let q = take n $ tst x; r = span (==0) q in (x,r) | x <- [0..n]]
