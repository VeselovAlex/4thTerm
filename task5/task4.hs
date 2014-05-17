prod :: Int -> [Int]
prod n = [1..n] >>= (\x -> ([1..n] >>= (\y -> (x * y) : [])))