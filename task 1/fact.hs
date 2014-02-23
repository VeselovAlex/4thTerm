factorial_rec :: Integer -> Integer
factorial_rec 0 = 1
factorial_rec x = x * factorial_rec (x - 1)