-- gfib :: (Num n, Ord n) => [n] -> [n -> n -> n] -> Int -> n
-- gfib edges funcs tn
--     | tn <= (length edges) = edges !! (tn - 1)
--     | otherwise = let step :: (Num n) => [n] -> [n -> n -> n] -> n
--                       step (arg:[]) [] = arg
--                       step (arg:args) (oper:opers) = oper arg (step args opers)
--                       in gfib ((tail edges) ++ [step edges funcs]) funcs (tn - 1)
-- 
-- fib :: (Num n, Ord n) => Int -> n
-- fib = let add :: (Num n) => n -> n -> n; add a b = a + b in gfib [0,1] [add]

gFib :: [a -> a -> a] -> [a] -> [a]
gFib fs (e:es) = let a = foldr ($) (last es) $ zipWith ($) fs (e:es)
                 in e:gFib fs (es ++ [a])

--fib :: Num a => Int -> a
--fib n = gfib [(+)] [0,1] !! (n - 1)

simpleGFib :: (a -> a -> a) -> a -> a -> [a]
simpleGFib f a b = a:simpleGFib f b (f a b)

fib :: Integral a => [a]
fib = simpleGFib (+) 0 1
