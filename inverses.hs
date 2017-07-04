import Primes
import Data.List

integers :: Int -> [Int]
integers n = [0..(n - 1)]

inverse :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
inverse operator identity n a = defaultIfNothing (-1) (find (\possibility -> (operator a possibility) == identity) (integers n))

modularOperator :: Int -> (Int -> Int -> Int) -> (Int -> Int -> Int)
modularOperator n operator = (\a b -> (operator a b) `mod` n)

modularAddition :: Int -> (Int -> Int -> Int)
modularAddition n = modularOperator n (+)

modularMultiplication :: Int -> (Int -> Int -> Int)
modularMultiplication n = modularOperator n (*)

findAdditiveInverses :: Int -> [(Int, Int)]
findAdditiveInverses n = [ (a, b) | a <- [0..n - 1], b <- [0..n - 1], (a + b) `mod` n == 0 ]

findMultiplicitiveInverses :: Int -> [(Int, Int)]
findMultiplicitiveInverses n = [ (a, b) | a <- [0..n - 1], b <- [0..n - 1], (a * b) `mod` n == 1 ]
