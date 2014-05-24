import Control.Monad
findGreatest:: (Eq a, Ord a) => [a] -> Maybe a
findGreatest [] = Nothing
findGreatest [x] = Nothing
findGreatest [x, y] = Nothing 
findGreatest l1@(x : l2@(y : xs)) = foldl1 (mplus) ((zip3 l1 l2 xs) >>= (\t -> [checkCondition t])) where
	checkCondition :: (Eq a, Ord a) => (a, a, a) -> Maybe a
	checkCondition (x, y, z) 
		| (y > x) && (y > z) = Just y
		| otherwise = Nothing