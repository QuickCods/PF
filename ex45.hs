{-
 - Ordenação por merg sort
 -}

-- a)

merge :: Ord a => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
                    | y < x = y:merge (x:xs) ys

-- b)

metades :: [a] -> ([a],[a])
metades [] = ([],[])
metades (x:xs) = (l1,l2)
                where
                  (ll1,ll2) = metades xs
                  l1 = x:ll2
                  l2 = ll1

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort l1) (msort l2)
          where
            (l1,l2) = metades l
