module Primes where
import Data.List

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], foldl (\prime number -> prime && ((x `mod` number) /= 0)) True [2..(x - 1)]]

defaultIfNothing :: a -> Maybe a -> a
defaultIfNothing defaultValue n = case n of
  Nothing -> defaultValue
  Just x -> x

lowestFactor :: Int -> Int
lowestFactor n = defaultIfNothing n (find (\potentialFactor -> n `mod` potentialFactor == 0) (primes (quot n 2)))

factors :: Int -> [Int]
factors n
  | n < 0 = []
  | n < 2 = [n]
  | otherwise = [(lowestFactor n)] ++ case quot n (lowestFactor n) of 1 -> []
                                                                      _ -> factors(quot n (lowestFactor n))