
--
expansion_list :: Int -> [[Int]]
expansion_list num = expansion_list' num num where
	expansion_list' 0 _ = [[]]
	expansion_list' 1 _ = [[1]]
	expansion_list' num bound = [(x : l) | x <- [1 .. top], l <- (recoursive x)] where
		top = min bound num
		new_bound x = min x bound
		recoursive x = (expansion_list' (num - x) (new_bound x))
		
--
check_constraint :: (a -> Bool) -> [a] -> Bool
check_constraint cons list = foldl (\x y -> x && (cons y)) True list

check_constraint_lazy :: (a -> Bool) -> [a] -> Bool
check_constraint_lazy cons [] = True 
check_constraint_lazy cons (x : xs) 
	| cons x = check_constraint_lazy cons xs
	| otherwise = False
--
compositions :: Int -> [a] -> [[a]]
compositions 0 _ = [[]]
compositions _[] = []
compositions n l = [(x : xs) | x <- l, xs <- (compositions (n - 1) l)]

compositions_123 :: Int -> [[Int]]
compositions_123  = (flip $ compositions) [1, 2, 3]

--
prod :: Int -> [Int]
prod n = [1..n] >>= (\x -> ([1..n] >>= (\y -> (x * y) : [])))


