{-# LANGUAGE ExistentialQuantification #-}
module Main (main) where

import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)
import Data.Maybe (maybe)
import qualified Data.Map as Map
import Fibonacci

type Shiftable a = [a] -> a
data FibFunction = forall a. (Show a, Read a) => F (Shiftable a)

fibOptions :: Show s => [(String, [s] -> Int -> IO ())]
fibOptions = [
    ("elem", (print .) . (!!)),
    ("list", ((putStrLn . unwords . map show) .) . flip take)]

fibFunctions :: [(String, FibFunction)]
fibFunctions = [
    ("add", F (foldr1 (+) :: Shiftable Integer)),
    ("mult", F (foldr1 (*) :: Shiftable Integer)),
    ("div", F (foldr1 (/) :: Shiftable Double)),
    ("log", F (foldr1 logBase :: Shiftable Double)),
    ("expi", F (foldr1 (^) :: Shiftable Integer)),
    ("expf", F (foldr1 (**) :: Shiftable Double)),
    ("concat", F (concat :: Shiftable String))] -- needs fixing

help :: IO ()
help = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ {-" (option)-}" (function) element ({edges})"
    putStrLn $ "\t" ++ name ++ " n = the nth fibonacci term,"
    putStrLn $ "\t" ++ name ++ " n {edges} = the nth fibonacci term for the specified edge cases,"
    putStrLn $ "\t" ++ name ++ " (add)|(mult)|(div)|(log)|(exp(i|f)|(concat) n {edges} = the nth fibonacci term for the specified edge cases and function"
    putStrLn $ "Note that arguments for 'concat' need to be surrounded by escaped double quotation marks. Also, 'concat' uses memory very quickly."

invalid args = do {putStrLn $ "Invalid input '" ++ args ++ "'"; fibIO ["help"]}

--parse args@(opt:f:n:edges) = case lookup opt fibOptions of
--    Just go -> case lookup f fibFunctions of
--	Just fn -> (go,fn,read n :: Int,edges)
--	Nothing -> parse $ opt:"add":tail args
--    Nothing -> parse $ "elem":args
--
--parse' args@(opt:f:n:edges) =
--    maybe (parse' $ "elem":args) (\go ->
--	(maybe (parse' $ opt:"add":tail args) (\fn ->
--	    (readMaybe n :: Maybe Int) >>= return . ((,,,) go fn $ map read edges)
--	) (lookup f fibFunctions))
--    ) (lookup opt fibOptions)
--
--parse'' args@(opt:f:n:edges) = case (lookup opt fibOptions, lookup f fibFunctions, readMaybe n :: Maybe Int) of
--    (Nothing,_,_) -> parse'' $ "elem":args
--    (Just _, Nothing, _) -> parse'' $ opt:"add":tail args
--    (Just _, Just _, Nothing) -> Nothing
--    (Just go, Just fn, Just i) -> Just (go, fn, i, map read edges)
--
--parse''' args@(opt:f:n:edges) = case (lookup opt fibOptions, lookup f fibFunctions, readMaybe n :: Maybe Int) of
--    (Nothing,_,_) -> parse''' $ "elem":args
--    (Just _, Nothing, _) -> parse''' $ opt:"add":tail args
--    (Just _, Just _, Nothing) -> Nothing
--    (Just go, Just (F fn), Just i) -> Just $ go (gfib fn $ map read edges) i

-- should make option to list terms
fibIO :: [String] -> IO ()
fibIO ["help"] = help
fibIO [n] = fibIO [n,"0","1"]
--	fibIO args@(opt:f:n:edges) = case lookup opt fibOptions of
--	    Just go -> putStrLn "halb" --case lookup f fibFunctions of
--	--	Just (F fn) -> putStrLn "blah" --go (gfib fn (map read edges)) (read n :: Int)
--	--	Nothing -> fibIO $ opt:"add":tail args
--fibIO args = case parse'' args of
--    Just (go, F fn, n, edges) -> go (gfib fn edges) n
--    Nothing -> invalid $ unwords args
fibIO args@(f:n:edges) = case lookup f fibFunctions of
    Just (F fn) -> maybe (invalid n) (print . (!!) (gfib fn $ map read edges)) (readMaybe n :: Maybe Int) --print $ gfib fn (map read params) !! ((read n) :: Int)
    Nothing -> fibIO $ "add":args
fibIO args = invalid $ unwords args

main = getArgs >>= fibIO
