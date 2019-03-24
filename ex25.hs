divprop :: Int -> [Int]
divprop n = [x | x <- [1..(n-1)], (n `mod` x) == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..(n-1)], sum (divprop x) == x]