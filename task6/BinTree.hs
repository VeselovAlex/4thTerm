module BinTree where

data BinarySearchTree a = Empty | Node { value  :: a
										, lchild ::(BinarySearchTree a)
										, rchild ::(BinarySearchTree a)
										} deriving (Eq)
instance (Show a) => Show(BinarySearchTree a) where
	show Empty = "Empty"
	show (Node x Empty Empty) = "(" ++ show x ++ ")"
	show (Node x Empty rc) = "(" ++ show x ++ " -> " ++ show rc ++ ")"
	show (Node x lc Empty) = "(" ++ show lc ++ " <- " ++ show x ++ ")"
	show (Node x lc rc) = "(" ++ show lc ++ " <- " ++ show x ++ " -> " ++ show rc ++ ")"

listToTree :: (Ord a) => [a] -> BinarySearchTree a
listToTree [] = Empty
listToTree (x : xs) = Node x (listToTree (filter (< x) xs)) (listToTree (filter (>= x) xs))
	
height :: BinarySearchTree a -> Int
height Empty = 0
height (Node _ lc rc) = (max (height lc) (height rc)) + 1

add :: (Show a, Ord a, Eq a) => a -> BinarySearchTree a -> BinarySearchTree a
add x Empty = Node x Empty Empty
add x (Node y left right)
	| x < y = Node y (add x left) right
	| otherwise = Node y left (add x right)
remove :: (Show a, Ord a, Eq a) => a -> BinarySearchTree a -> BinarySearchTree a
remove x Empty = Empty
remove x (Node y left right)
	| x > y = Node y left (remove x right)
	| x < y = Node y (remove x left) right
	| left == Empty = right
	| right == Empty = left
	| otherwise = Node (leftMost right) left (remove (leftMost right) right) where
		leftMost (Node y Empty _) = y
		leftMost (Node _ left _) = leftMost left

size :: (Show a, Ord a, Eq a) => BinarySearchTree a -> Int
size Empty = 0
size (Node y left right) = (size $ left) + (size $ right) + 1

element :: (Show a, Ord a, Eq a) => a -> BinarySearchTree a -> Bool
element _ Empty = False
element x (Node y left right)
	| x > y = element x right
	| x < y = element x left
	| otherwise = True
	
	
		


 
 
 