area :: (Num a, Fractional a, Floating a) => a -> a -> a -> a
area a b c = sqrt(s * (s - a) * (s - b) * (s - c))
								where s = (a + b + c)/2
