expansion_list :: Int -> [[Int]]
expansion_list num = expansion_list' num num where
	expansion_list' 0 _ = [[]]
	expansion_list' 1 _ = [[1]]
	expansion_list' num bound = [(x : l) | x <- [1 .. top], l <- (recoursive x)] where
		top = min bound num
		new_bound x = min x bound
		recoursive x = (expansion_list' (num - x) (new_bound x))