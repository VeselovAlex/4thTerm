--Task 4.3
even_quantity :: (Integral a) => [a] -> Int
even_quantity = length . (filter (even))

even_quantity' :: (Integral a) => [a] -> a
even_quantity' = foldr (\y x -> x + (mod (y + 1) 2)) 0 

even_quantity'' :: (Integral a) => [a] -> a
even_quantity'' = sum . map (((flip $ mod) 2) . (+ 1))

--Task 4.4
check_unique :: (Eq a) => [a] -> Bool
check_unique [] = True
check_unique (x:xs) = (not $ elem x xs) && (check_unique xs)
