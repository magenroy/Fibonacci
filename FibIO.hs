module Main (main) where

import System.Environment (getArgs)
import Fibonacci

usage :: IO ()
usage = do
	putStrLn "one input for the nth fibonacci term,"
	putStrLn "3 inputs for the nth fibonacci term with a and b as edge cases,"
	putStrLn "(add)|(mult)|(div)|(log)|(exp(i|f)|(concat) and 3 inputs for the nth fibonacci term with a and b as edge cases for function f"


-- should make option to list terms
-- this is obviously not ideal at the moment (so much copy and paste)
fibIO :: [String] -> IO ()
fibIO [] = fibIO ["help"]
fibIO ["help"] = usage
fibIO [n] = print (fib !! (read n))
fibIO [n,a,b] = print (gfib (+) ((read a) :: Integer) ((read b) :: Integer) !! ((read n) :: Int))
fibIO ["add",n,a,b] = print (gfib (+) ((read a) :: Integer) ((read b) :: Integer) !! ((read n) :: Int))
fibIO ["mult",n,a,b] = print (gfib (*) ((read a) :: Integer) ((read b) :: Integer) !! ((read n) :: Int))
fibIO ["div",n,a,b] = print (gfib (/) ((read a) :: Double) ((read b) :: Double) !! ((read n) :: Int))
fibIO ["log",n,a,b] = print (gfib logBase ((read a) :: Double) ((read b) :: Double) !! ((read n) :: Int))
fibIO ["expi",n,a,b] = print (gfib (^) ((read a) :: Integer) ((read b) :: Integer) !! ((read n) :: Int))
fibIO ["expf",n,a,b] = print (gfib (**) ((read a) :: Double) ((read b) :: Double) !! ((read n) :: Int))
fibIO ["concat",n,a,b] = print (gfib (++) ((read a) :: String) ((read b) :: String) !! ((read n) :: Int))

main :: IO ()
main = do
	args <- getArgs
	fibIO args
