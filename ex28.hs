dotprod :: [Float] -> [Float] -> Float
dotprod [] l2 = 0
dotprod l1 [] = 0
dotprod (x:xs) (y:ys) = (x*y) + (dotprod xs ys)
