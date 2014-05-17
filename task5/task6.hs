import Polynome
class Diff a where
	dx :: a -> a
	
round' :: Float -> Float
round' x = (/100) $ fromInteger $ round $ (*100) $ x

instance Diff(Monome) where
	dx (Monome(a, b)) = Monome(round'(a * (fromIntegral b)), b - 1)
instance Diff(Polynome) where
	dx (Polynome x) = simplify $ Polynome $ map (dx) x