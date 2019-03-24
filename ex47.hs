permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ps | p <- permutations xs, ps <- distribuir x p]

distribuir x [] = [[x]]
distribuir x (y:ys) = (x:y:ys) : (map (y:) (distribuir x ys))
