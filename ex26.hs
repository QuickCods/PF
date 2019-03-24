divprop :: Int -> [Int]
divprop n = [x | x <- [1..(n-1)], (n `mod` x) == 0]

primo :: Int -> Bool
primo n | (length (divprop n)) > 1 || n < 2 = False
        | otherwise = True