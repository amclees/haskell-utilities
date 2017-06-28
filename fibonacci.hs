fibonacciRecursive :: Int -> Int -> [Int] -> [Int]
fibonacciRecursive a b existing = existing ++ [a + b] ++ fibonacciRecursive b (a + b) []

fibonacci :: [Int]
fibonacci = fibonacciRecursive 1 1 [1, 1]