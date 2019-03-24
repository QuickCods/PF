-- a)

max3 :: Ord a => a -> a -> a -> a
max3 x y z | (x >= y) && (x >= z) = x
           | (y >= x) && (y >= z) = y
           | otherwise = z

min3 :: Ord a => a -> a -> a -> a
min3 x y z | (x <= y) && (x <= z) = x
           | (y <= x) && (y <= z) = y
           | otherwise = z

-- b)

max3' ::
max3' x y z = max x max(y z)

min3' ::
min3' x y z = min x min(y z)
