-- a)

fact n = product [1..n]
binom n k = num `div` den
			where
				num = fact n
				den = (fact k) * (fact (n-k))

-- b)

binom_fast n k = num `div` den
            where
                num = product [n-k+1..n]
                den = product [1..k]

