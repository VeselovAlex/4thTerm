import BinTree

--Task 1 - DONE
prim_list :: (Integral a) => [a]
prim_list = 2 : 3 : [x | x <- [4..], (filter (\y -> 0 == (mod x y)) [2.. (div x 2)]) == [] ]

--Task 2 - DONE
filter0 :: (Ord a, Num a) => BinaryTree a -> [a]
filter0 t = tfilter' (< 0) t []

--Task 3 - DONE
elemPos :: (Eq a) => a -> [a] -> Int
elemPos x xs 
	| elemList == [] = error "No such element"
	| otherwise = fst $ head $ elemList where
		elemList = filter (\(_, z) -> z == x) (zip [1..] xs)

--Task 4 - DONE
someFraction :: (Floating a) => [a] -> a
someFraction xs = sf xs 0 1 where
	sf [] acc_sum acc_cos = acc_sum / acc_cos
	sf (x : xs) acc_sum acc_cos = sf xs (acc_sum + x) (acc_cos * (cos x))
	
--Task 5
data Press = Book { title :: String
					,author :: String
					,cost :: Int
					}
			 | Magazine { title :: String
						  ,year :: Int
						  ,number :: Int
						  ,cost :: Int
						  } deriving (Show)
						
totalCost :: [Press] -> Int
totalCost x = totalcost' x 0 where
	totalcost' [] acc = acc
	totalcost' (x : xs) acc = totalcost' xs (acc + cost (x))
	
examplePress :: [Press]
examplePress = [(Book "abc" "dvkj" 5), 
				(Magazine "sjbj" 2004 10 2), 
				(Book "hjjjo" "rykuki" 8)
				] 