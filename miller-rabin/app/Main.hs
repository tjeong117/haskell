module Main where

import System.Random (randomRIO)
import Data.Bits
import Control.Monad (replicateM)

-- Perform modular multiplication using binary method to avoid overflow
modMul :: Integer -> Integer -> Integer -> Integer
modMul a b m = go a b 0 m
  where
    go _ 0 res _ = res
    go x y acc m
      | y `testBit` 0 = go x' y' ((acc + x) `mod` m) m
      | otherwise     = go x' y' acc m
      where
        x' = (x `shiftL` 1) `mod` m
        y' = y `shiftR` 1

-- Modular exponentiation using square-and-multiply algorithm with bitwise operations
modExp :: Integer -> Integer -> Integer -> Integer
modExp base exp m
  | exp == 0  = 1
  | otherwise = go base exp 1 m
  where
    go _ 0 res _ = res
    go x y acc m
      | y `testBit` 0 = go x' y' ((modMul acc x m) `mod` m) m
      | otherwise     = go x' y' acc m
      where
        x' = modMul x x m
        y' = y `shiftR` 1

-- Find the values of s and d where n-1 = d * 2^s
decompose :: Integer -> (Int, Integer)
decompose n = go 0 (n-1)
  where
    go s d
      | d `testBit` 0 = (s, d)
      | otherwise     = go (s+1) (d `shiftR` 1)

-- Single iteration of Miller-Rabin test
millerRabinIteration :: Integer -> Integer -> Bool
millerRabinIteration n a
  | n == 2    = True
  | even n    = False
  | otherwise = let
      (s, d) = decompose n
      x = modExp a d n
      in if x == 1 || x == n-1
         then True
         else witnessLoop (s-1) x
  where
    witnessLoop s x
      | s < 0     = False
      | x == n-1  = True
      | otherwise = witnessLoop (s-1) ((modMul x x n) `mod` n)

-- Full Miller-Rabin test with k iterations
isPrime :: Integer -> Int -> IO Bool
isPrime n k
  | n <= 1    = return False
  | n <= 3    = return True
  | otherwise = do
      witnesses <- replicateM k (randomRIO (2, n-2))
      return $ all (millerRabinIteration n) witnesses

main :: IO ()
main = do
    putStrLn "Enter a number to test for primality:"
    input <- getLine
    let number = read input :: Integer
    let k = 20  -- Number of iterations
    result <- isPrime number k
    putStrLn $ show number ++ " is " ++ if result then "probably prime" else "composite"
