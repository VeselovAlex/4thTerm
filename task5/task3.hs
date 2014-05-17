compositions :: Int -> [a] -> [[a]]
compositions 0 _ = [[]]
compositions _[] = []
compositions n l = [(x : xs) | x <- l, xs <- (compositions (n - 1) l)]

compositions_123 :: Int -> [[Int]]
compositions_123  = (flip $ compositions) [1, 2, 3]