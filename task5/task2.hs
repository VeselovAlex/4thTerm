check_constraint :: (a -> Bool) -> [a] -> Bool
check_constraint cons list = foldl (\x y -> x && (cons y)) True list

check_constraint_lazy :: (a -> Bool) -> [a] -> Bool
check_constraint_lazy cons [] = True 
check_constraint_lazy cons (x : xs) 
	| cons x = check_constraint_lazy cons xs
	| otherwise = False