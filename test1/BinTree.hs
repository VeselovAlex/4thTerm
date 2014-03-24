module BinTree where

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

--Task 4.1
tfilter :: (Ord a) => (a -> Bool) -> BinaryTree a -> BinaryTree a
tfilter _ (Empty) = Empty
tfilter f t@(Node a lc rc) = listToTree(tfilter' f t [])

tfilter' :: (Ord a) => (a -> Bool) -> BinaryTree a -> [a] -> [a]
tfilter' _ (Empty) l = l
tfilter' f t@(Node a lc rc) l 
	| f a = a : other
	| otherwise = other where
		other = (tfilter' f lc (tfilter' f rc l))

--Task 4.2
tfoldl_inorder :: (b -> a -> b) -> b -> BinaryTree a -> b
tfoldl_inorder _ acc Empty = acc
tfoldl_inorder f acc (Node a lc rc)= tfoldl_inorder f (f (tfoldl_inorder f acc rc) a) lc

tfoldr_inorder :: (a -> b -> b) -> b -> BinaryTree a -> b
tfoldr_inorder _ acc Empty = acc
tfoldr_inorder f acc (Node a lc rc)= tfoldr_inorder f (f a (tfoldr_inorder f acc lc)) rc

tfoldl_preorder :: (b -> a -> b) -> b -> BinaryTree a -> b
tfoldl_preorder _ acc Empty = acc
tfoldl_preorder f acc (Node a lc rc)= f (tfoldl_preorder f (tfoldl_preorder f acc rc) lc) a

tfoldr_preorder :: (a -> b -> b) -> b -> BinaryTree a -> b
tfoldr_preorder _ acc Empty = acc
tfoldr_preorder f acc (Node a lc rc)= f a (tfoldr_preorder f (tfoldr_preorder f acc lc) rc)

tfoldl_postorder :: (b -> a -> b) -> b -> BinaryTree a -> b
tfoldl_postorder _ acc Empty = acc
tfoldl_postorder f acc (Node a lc rc)= tfoldl_postorder f (tfoldl_postorder f (f acc a) rc) lc

tfoldr_postorder :: (a -> b -> b) -> b -> BinaryTree a -> b
tfoldr_postorder _ acc Empty = acc
tfoldr_postorder f acc (Node a lc rc)= tfoldr_postorder f (tfoldr_postorder f (f a acc) lc) rc

 