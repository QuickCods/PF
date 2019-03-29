--a
myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs
--(if x then True else myand xs)

--b
myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

--c
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

--d
myreplicate :: Int -> a -> [a]
myreplicate n x | n == 0 = []
                | otherwise = x : myreplicate (n-1) x

--e
(!!) :: [a] -> Int -> a
(!!) xs n = head(drop n xs)

--f (nao funciona)
myelem :: Eq a => a -> [a] -> Bool
myelem n [x] = n == x
myelem n (x:xs) = n == x || myelem n xs
