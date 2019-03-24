--a
factorial 1 = 1
factorial n = n * fact(n-1)

--b
rangeProduct n m | n > m = n * (rangeProduct (n-1) m)
                 | otherwise = m

--c
fact n = rangeProduct n 1
