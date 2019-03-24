metades :: [a] -> ([a],[a])
metades k = (esq, dir)
			where
				n1 = length k `div` 2
				esq = take n1 k
				dir = drop n1 k 