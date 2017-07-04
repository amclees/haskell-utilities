import Primes
import Data.List

validGoldbachPartition :: Int -> (Int, Int) -> Bool
validGoldbachPartition n (a, b) = (n * 2) == (a + b)

goldbachPartition :: Int -> (Int, Int)
goldbachPartition n = defaultIfNothing (0, 0) (find (validGoldbachPartition n) [(a, b) | a <- primes(n * 2), b <- primes(n * 2)])