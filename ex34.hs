anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 = (f 0) == 0
anyZero f n = f n == 0 || anyZero f (n-1)
