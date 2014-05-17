data BinaryCharTree = Empty  
					| Node {
							value :: Char, 
							left :: BinaryCharTree, 
							right :: BinaryCharTree
						}
--this isn't the function you're looking for :)						
stringToBCT :: String -> BinaryCharTree
stringToBCT "" = Empty
stringToBCT (x : xs) = Node x (stringToBCT $ filter (< x) xs) (stringToBCT $ filter (>= x) xs)

instance Show(BinaryCharTree) where
	show Empty = "e"
	show (Node x left right) = 'n' : x : (show $ left) ++ (show $ right)

readTree :: String ->  BinaryCharTree
readTree s 
	|((snd $ readNode $ s )== "") = fst $ readNode $ s
	| otherwise = error "Parse error"


readNode :: String -> (BinaryCharTree, String)
readNode ('e' : xs) = (Empty, xs)
readNode ('n' : x : xs) = (Node x (fst $ leftNodeWrap) (fst $ rightNodeWrap), (snd $ rightNodeWrap)) where
	leftNodeWrap = readNode(xs)
	rightNodeWrap = readNode $ snd $ leftNodeWrap
readNode otherwise = error "Parse error"
