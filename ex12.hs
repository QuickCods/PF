-- a)

curta :: [a] -> Bool
curta l = if (length l) <= 2 then True else False

-- b)

curta' :: [a] -> Bool
curta' [] = True
curta' (x:[]) = True
curta' (x:y:[]) = True
curta' _ = False
