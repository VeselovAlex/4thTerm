module Polynome where

newtype Monome = Monome(Float, Int)
instance Eq(Monome) where
	Monome (a, b) == Monome (c, d) = (a == c) && (b == d)
instance Show(Monome) where
	show (Monome (0, _)) = "0" 
	show (Monome((-1),1)) = "-x"
	show (Monome(1,1)) = "x"
	show (Monome(x,0)) = show x
	show (Monome(1,y)) = "x^" ++ show y
	show (Monome(x,1)) = show x ++ "x"
	show (Monome((-1),y)) = "-x^" ++ show y
	show (Monome(x,y)) = show x ++ "x^" ++ show y
instance Ord(Monome) where
	Monome (a, b) <= Monome (c, d) 
		| b == d = a <= c
		| otherwise = b <= d
instance Num(Monome) where
	Monome (a, b) + Monome (c, d)
		| b == d = Monome ((a + c), b)
		| otherwise = error "Could not add monomes in different powers"
	Monome (a, b) * Monome (c, d) = Monome ((a * c), (b + d))
	abs(Monome (a, b)) = Monome((abs a), b)
	signum (Monome (a, b)) = Monome ((signum a), 0)
	fromInteger a = Monome ((fromInteger a) :: Float, 0)
	
newtype Polynome = Polynome [Monome]

qsort :: Polynome -> Polynome
qsort (Polynome x) = Polynome (qsort' x)
qsort' :: [Monome] -> [Monome]
qsort' [] = [] 
qsort' (y : ys) = (qsort' (filter (>= y) ys)) ++ [y] ++ (qsort' (filter (< y) ys))

simplify :: Polynome -> Polynome
simplify (Polynome x) = Polynome (simplify'(qsort' x)) where
	simplify' (x@(Monome(a, b)) : y@(Monome(c, d)) : xs)
		| b == d = simplify'((x + y) : xs)
		| otherwise = x : simplify'(y : xs)
	simplify' x = x
	
	
instance Eq(Polynome) where
	(Polynome x) == (Polynome y) = (qsort' x) == (qsort' y)
instance Ord(Polynome) where
	z <= y = (p_head $ qsort $ z) <= (p_head $ qsort $ y) where
		p_head (Polynome (x : xs)) = x
instance Show(Polynome) where
	show (Polynome []) = "Empty"
	show (Polynome (x : xs)) = (show x) ++ (show'(Polynome xs)) where
		show' (Polynome []) = ""
		show' (Polynome (x : xs)) 
			| (signum x) > 0 = "+" ++ show x ++ (show'(Polynome xs)) 
			| (signum x) == 0 = show'(Polynome xs)
			| otherwise = show x ++ (show'(Polynome xs))
instance Num(Polynome) where
	(Polynome x) + (Polynome y) = simplify $ Polynome $ (x ++ y)
	(Polynome x) * (Polynome y) = simplify $ Polynome $ [a * b | a <- x, b <- y]
	abs(Polynome x) = Polynome (map (abs) x)
	signum (Polynome x) = Polynome [signum $ maximum $ x]
	fromInteger a = Polynome [(fromInteger a) :: Monome]
	



