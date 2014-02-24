-- Задание 1 
my_reverse :: [a] -> [a]
my_reverse = foldl (\res x -> x:res) []

-- Задание 2
deg_2 :: Integer -> [Integer]
deg_2 0 = []
deg_2 1 = [2]
deg_2 n = (head(degs) * 2) : degs
	where degs = deg_2 (n - 1)

-- Задание 3
sum_dig :: (Integral a) => a -> a
sum_dig 0 = 0
sum_dig x = x `mod` 10 + sum_dig (x `div` 10)

-- Задание 4
first_pos :: (Eq a, Integral b) => a -> [a] -> b
first_pos _ [] = error "No such element"
first_pos y (x:xs)
	| y == x = 0 -- Начинает с 0
	| otherwise  = (first_pos y xs) + 1
	
-- Задание 5
is_palindrome :: String -> Bool
is_palindrome s = s == my_reverse s