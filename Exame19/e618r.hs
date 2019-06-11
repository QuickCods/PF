{-
1 a) = length ( [[],[1,2],[],[]] ) = 4
  b) [5,4,3]
  c) 6
  d) [(0,1),(0,3),(1,2),(1,4),(2,3),(3,4)]
  e) [1,9,25,49,81]
  f) 6
  g) [(2*x,2^x-1) | x <- [1..]]                       ???
  h) 59
  i) [a] -> Bool
  j) (Ord a, Num a) => [a -> Bool]
  k) data Arv = Folha | No Arv Int Arv
  l)
-}

--2
pontuacao :: [(String,Int,Int,Int)] -> [(String,Int)]
pontuacao xs = map pont xs
        where pont (n, v, e, d) = (n, 3*v+e)

njogos :: [(String, Int, Int, Int)] -> Int -> Bool
njogos xs n = and (map jogos xs)
        where jogos (_, v, e, d) = v + e + d == n
--ou
njogos' :: [(String, Int, Int, Int)] -> Int -> Bool
njogos' xs n = all (==n) (map (\(_, v, e, d) -> v + e + d) xs)

--3
-- crescente :: IO()
-- crescente = do x <- getLine
--         crescenteAux (read x) 1

-- crescenteAux :: Int -> Int -> IO()
-- crescenteAux x n = do y <- getLine
--         let y' = read y
--         in if x <= y' then crescenteAux y' (n+1)
--           else print n
--ou
-- crescente = do x <- getLine
--         n <- crescenteAux (read x) 1
--         print n

-- crescenteAux :: Int -> Int -> IO Int
-- crescenteAux x n = do x <- getLine
--         let y' = read y
--         in if x <= y' then crescenteAux y' (n+1)
--           else return n

--6
data ArvC a = Vazia | No a Int (ArvC a) (ArvC a)

-- a
nelem :: ArvC a -> Int
nelem Vazia = 0
nelem (No _ _ esq dir) = 1 + nelem esq + nelem dir

--b
eElem :: ArvC a -> Int
eElem Vazia = 0
eElem (No _ n _ _) = n

update :: ArvC a -> ArvC a
update Vazia = Vazia
update (No x _ esq dir) = (No x n esq' dir')
      where esq' = update esq
            dir' = update dir
            n = 1 + eElem esq' + eElem dir'
