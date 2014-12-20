module Fibonacci (gfib) where

import Control.Applicative
import Data.List (inits, tails)

binaryGFib :: (a -> a -> a) -> a -> a -> [a]
binaryGFib f a b = a:binaryGFib f b (f a b)
binaryGFib' f a b = let seq = binaryGFib' f a b in a:b:zipWith f seq (tail seq)

itaiGFib :: ([a] -> a) -> [a] -> [a]
itaiGFib f init@(x:xs) = x:itaiGFib f (xs ++ [f init])

test f prep app head tail init = prep (head init) $ test f prep app head tail (app (tail init) (f init))

gFib :: ([a] -> [a]) -> [a] -> [a]
gFib f init = init ++ (drop (length init) $ gFib f $ init ++ (f init))
gFib' f init = let next f' init' = f' init' ++ (next f' $ init' ++ (f' init')) in init ++ (next f init)

gfib = ggfib --binaryGFib

ggfib :: ([a] -> a) -> [a] -> [a]
ggfib = sequence . scanl (\g f list@(_:xs) -> g $ xs ++ [f list]) head . repeat -- should generalize to general collections that can "shift"
