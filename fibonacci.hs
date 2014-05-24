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
-- 
gfib :: [a -> a -> a] -> [a] -> [a]
gfib fs (e:es) = let a = foldr ($) (last es) $ zipWith ($) fs (e:es)
                 in e:gfib fs (es ++ [a])

fib :: Num a => Int -> a
fib n = gfib [(+)] [0,1] !! (n - 1)

kieranFib :: [Integer]
kieranFib = go 0 1
  where go a b = a:go b (a + b)

fibIO :: IO ()
fibIO = do putStrLn "Term number: "
           n <- getLine
           putStrLn (show (fib ((read n) :: Int)))

main :: IO ()
main = fibIO
