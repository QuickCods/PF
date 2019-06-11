{-
1 a) ["abc", "", "dce"]
  b) [[2],[],[3],[4]]
  c) 10
  d) [2,4,6,8]
  e) [1,0,-1,-2,-3]
  f) [6,7,8,9,10]
  g) [4,5,6]
  h) [(-2)^x | x <- [1..]]
  i) 24
  j) [a] -> a      --maybe dunno          ???
  k) ([Bool],[Char])
  l) aval :: E a -> (a -> Bool) -> (Bool -> Bool -> Bool) -> Bool
  m) concat :: [[a]] -> [a]
-}

--2
-- aprov :: [Int] -> [Char]
-- aprov xs = [if x < 15 then 'R' else 'A' | x <- xs]

-- injust :: [Char] -> Int
-- injust xs = length [x | x <- xs, x > 9 && xs < 15]

--3
--repete a = [] : map (a:) (repete a)

--5
--recursivamente
-- compL [] v = v
-- compL (f:fs) v = f (compL fs v)
--
--ordem superior
-- compL fs v = foldr (.) id fs v
--ou
-- compL fs v = foldr apply v fs
--         where apply x y = xy

--6
data Arv a = Vazia | No a (Arv a) (Arv a)

soma :: Num a => Arv a -> a
soma Vazia = 0
soma (No x esq dir) = x + soma esq + soma dir

foldtree :: (a -> b -> b -> b) -> b -> Arv a -> b
foldtree f v Vazia = v
foldtree f v (No x esq dir) = f x (foldtree f v esq) (foldtree f v dir)

-- 7 exame
{-
  Para qualquer árvore numérica
    soma t = foldtree soma3 0 t
  Por indução sobre t
    - caso base t = Vazia
    soma Vaia = 0 = foldtree soma 0 Vazia
           soma1  foldtree1
    - caso indutivo t = No x esq dir
    Suponhamos que soma esq = foldtree soma 3 0 esq e soma dir = foldtree soma 0 dir
    queremos mostrar que soma (No x esq dir) = foldtree soma3 0 (No x esq dir)
    soma (No x esq dir) = x + soma esq + soma dir = x + (foldtree soma3 0 esq) + (foldtree soma3 0 dir) =
                      soma2                      H.I.                                                  soma3
    soma3 x (foldtree soma3 0 esq) (foldtree soma3 0 dir) = foldtree soma3 0 (No x esq dir)
                                                      foldtree2
-}
