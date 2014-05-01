list_process :: [Integer] -> IO()
list_process l = do
	putStrLn "List processor: Type command (0 - exit, 1 - add, 2 - remove, 3 - print):" 
	command <- getLine
	case command of
		"0" -> do
			putStrLn "Exiting..."
			return ();
		"1" -> do
			putStrLn "Enter value to add: "
			value <- readLn
			list_process (add value l)
		"2" -> do
			putStrLn "Enter value to remove: "
			value <- readLn
			list_process (remove value l)
		"3" -> do
			putStrLn ("List: " ++ show(l))
			list_process l
		otherwise -> do
			putStrLn ("Invalid command.")
			list_process l	
main =	list_process [];
	
add :: (Ord a) => a -> [a] -> [a]
add x [] = [x]
add x l@(y : ys)
	| x > y = y : (add x ys)
	|otherwise = x : l
	
remove :: (Eq a, Ord a) => a -> [a] -> [a]
remove _ [] = []
remove x l@(y : ys)
	| x == y = ys
	| x > y = y : remove x ys
	| otherwise = l
	