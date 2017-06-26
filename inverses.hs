findAdditiveInverses :: Int -> [(Int, Int)]
findAdditiveInverses n = [ (a, b) | a <- [0..n - 1], b <- [0..n - 1], (a + b) `mod` n == 0 ]

findMultiplicitiveInverses :: Int -> [(Int, Int)]
findMultiplicitiveInverses n = [ (a, b) | a <- [0..n - 1], b <- [0..n - 1], (a * b) `mod` n == 1 ]