maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f 0 = f 0
maxFun f n = max (f n) (maxFun f (n-1))
