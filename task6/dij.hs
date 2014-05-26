import Control.Monad
data Graph v e = Graph [(Int, v)] [(Int, Int, e)] deriving (Show)

data Distance a = Infinite |Length a deriving (Eq)

instance (Show a) => Show(Distance a) where
	show Infinite = "oo"
	show (Length x) = show x

instance Monad Distance where
	Infinite >>= f = Infinite
	Length x >>= f = f x
	return x = Length x
	
instance (Ord a) => Ord (Distance a) where
	Infinite <= Infinite = True
	Infinite <= (Length _) = False
	(Length x) <= (Length y) = x <= y
	
ex1 :: Graph (Distance Int) Int
ex1 = Graph ((1 , Length 0) : [(x, Infinite)| x <- [2..5]]) 
	[(1, 2, 1), (1, 3, 3), (1, 5, 2), (2, 3, 4), (3, 4, 2), (3, 5, 1), (2, 4, 5)]

isIncidentTo :: (Int, v) -> (Int, Int, e) -> Bool
isIncidentTo (a, _) (b, c, _) = (a == b) || (a == c)

isOppositeTo :: (Int, v) -> (Int, Int, e) -> Int
isOppositeTo (a, _) (b, c, _) 
	|(a == b) = c
	|(a == c) = b
	| otherwise = error "Invalid vertex"

edgeLength :: (Int, Int, e) -> e
edgeLength (_, _, l) = l

addDistance :: (Num a) => Distance a -> a -> Distance a
addDistance (Length x) y = Length (x + y)
addDistance Infinite _ = Infinite

markUpVertex :: (Ord a) => (Int, a) -> a -> (Int, a)
markUpVertex (n, x) y = (n, min x y)

getVertex :: [(Int, a)] -> Int -> (Int, a)
getVertex [] _ = error "No such vertex"
getVertex (v@(n, _) : vs) m
	| n == m = v
	| otherwise = getVertex vs m

replaceVertex :: [(Int, a)] -> (Int, a) -> [(Int, a)]
replaceVertex [] _ =  []
replaceVertex (v@(n, _) : vs) v2@(m, _)
	| n == m = v2 : vs
	| otherwise = v : (replaceVertex vs v2)	

sortBySnd :: (Ord a) => [(b, a)] -> [(b, a)]
sortBySnd [] = []
sortBySnd [x] = [x]
sortBySnd (x : xs) = sortBySnd (filter ((< (snd x)).snd) xs) ++ [x] ++ sortBySnd (filter ((>= (snd x)).snd) xs)

markUpFront :: Graph (Distance Int) Int -> [(Int, Distance Int)]
markUpFront (Graph [] _) = []
markUpFront (Graph [v] _) = []
markUpFront (Graph (v : vs) es) = (foldl (replaceVertex) vs) $ 
	(filter (isIncidentTo v) es) >>= 
	(\e -> [markUpVertex (getVertex vs (isOppositeTo v e)) (addDistance (snd v) (edgeLength  e))])

	
markUpFront' :: Graph (Distance Int) Int -> [(Int, Distance Int)] -> ([(Int, Distance Int)], [(Int, Distance Int)])
markUpFront' (Graph [] _, p) = ([], p)
markUpFront' (Graph [v] _, p) = ([], p)
markUpFront' (Graph (v : vs) es) = (res@(foldl (replaceVertex) vs) $ 
	(filter (isIncidentTo v) es) >>= 
	(\e -> [markUpVertex (getVertex vs (isOppositeTo v e)) (addDistance (snd v) (edgeLength  e))]), updatePath vs res path v)
	
prependVertex :: Graph (Distance Int) Int -> (Int, Distance Int) -> Graph (Distance Int) Int
prependVertex (Graph vs es) v = Graph (v : vs) es 

restoreEdges :: Graph (Distance Int) Int -> [(Int, Int, Int)] -> Graph (Distance Int) Int
restoreEdges (Graph vs es) nes = Graph vs nes
	
markUpGraph' :: Graph (Distance Int) Int -> Graph (Distance Int) Int
markUpGraph' g@(Graph [] _) = g
markUpGraph' g@(Graph (v : vs) es) = prependVertex (markUpGraph' $ Graph (sortBySnd $ markUpFront g) (filter (not . isIncidentTo v) es)) v

markUpGraph :: Graph (Distance Int) Int -> Graph (Distance Int) Int
markUpGraph g@(Graph _ es) = restoreEdges (markUpGraph' g) es

updatePath :: [(Int, Distance Int)] -> [(Int, Distance Int)] -> [(Int, Distance Int)] -> (Int, Distance Int) -> [(Int, Distance Int)]
updatePath x y z v = zipWith3 (\a b c -> if (a == b) then c else v) x y z
