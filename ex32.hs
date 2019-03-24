procura_raiz :: Integer -> Integer -> Integer
procura_raiz n r | r^2 == n               = r
                 | r^2 < n && (r+1)^2 > n = r
                 | r^2 < n = procura_raiz n (r+1)

raisq :: Integer -> Integer
raisq x = procura_raiz x 0
