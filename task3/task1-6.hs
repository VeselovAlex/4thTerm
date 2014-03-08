
-- Task 1
func :: Num a => a -> [a] -> [a]
func = map.(*)

-- Task 2
deg_2 :: Integral a => [a]
deg_2 = 1: (map (* 2) deg_2)

-- Task 3
list_179 :: Integral a => [a]
list_179 = 1 : 7 : 9 : [10 * x + y | x <- list_179, y <- [1, 7, 9]]

-- Task 4
fst_pos :: (Eq a, Ord a, Num a) => [a] -> Int
fst_pos l@(x : y : xs) = snd $ head $ filter (\x -> ((fst x) == max_sum)) (zip sums [1..]) where
	sums = dif_list l	
	max_sum = maximum sums
fst_pos _ = error " Not enough data" 
	
dif_list :: (Num a) => [a] -> [a]
dif_list (z : zs) = reverse $ tail $ foldl (\(x : xs) y -> (y : (x + y) : xs)) [z] zs

-- Task 5
data BinaryTree a = Empty | Node { value  :: a
								 , lchild ::(BinaryTree a)
								 , rchild ::(BinaryTree a)
								 } 
								 
instance (Show a) => Show(BinaryTree a) where
	show Empty = "Empty"
	show (Node x Empty Empty) = "(" ++ show x ++ ")"
	show (Node x Empty rc) = "(" ++ show x ++ " -> " ++ show rc ++ ")"
	show (Node x lc Empty) = "(" ++ show lc ++ " <- " ++ show x ++ ")"
	show (Node x lc rc) = "(" ++ show lc ++ " <- " ++ show x ++ " -> " ++ show rc ++ ")"

listToTree :: (Ord a) => [a] -> BinaryTree a
listToTree [] = Empty
listToTree (x : xs) = Node x (listToTree (filter (< x) xs)) (listToTree (filter (>= x) xs))
	
height :: BinaryTree a -> Int
height Empty = 0
height (Node _ lc rc) = (max (height lc) (height rc)) + 1

-- Task 6
brackets :: String -> String
brackets = filter ((flip elem) bracket_list) where
	bracket_list = "()[]{}<>"

is_complement :: Char -> Char -> Bool	
is_complement x y = (is_complement' x y) || (is_complement' y x) where
	is_complement' '(' ')' = True
	is_complement' '[' ']' = True
	is_complement' '{' '}' = True
	is_complement' '<' '>' = True
	is_complement' _ _     = False

is_correct_brackets :: String -> Bool
is_correct_brackets s  = (check $ brackets s) == [] where	
	check = foldl (stack_op) [] where
		stack_op [] x = [x]
		stack_op (s : ss) x
			|is_complement x s = ss
			|otherwise = (x : s : ss)		
 