import Stack
--49
maismais :: [a] -> [a] -> [a]
maismais xs ys = foldr (:) ys xs

myconcat :: [[a]] -> [a]
myconcat xs = foldr (++) [] xs

-- myreverser :: [a] -> [a]
-- myreverser xs = foldr

-- myreversel :: [a] -> [a]
-- myreversel

-- myelem :: Eq a => a -> [a] -> Bool
-- myelem n xs = foldr any [] xs

--50
dec2int :: [Int] -> Int
dec2int xs = foldl (\x y -> 10*x+y) 0 xs

--53
shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotate :: [a] -> [[a]]
rotate xs = take (length xs) (iterate shift xs)

--54
--a
-- lmax :: Ord a => [a] -> a
-- lmax xs = foldl max (head xs) (tail xs)

-- lmax1 :: Ord a => [a] -> a
-- lmax1 xs = foldl max xs

-- lmin :: Ord a => [a] -> a
-- lmin xs = foldl min (head xs) (tail xs)

-- lmin1 :: Ord a => [a] -> a
-- lmin1 xs = foldl min xs

-- rmax :: Ord a => [a] -> a
-- rmax xs = foldr max (last xs) (init xs)

-- rmax1 :: Ord a => [a] -> a
-- rmax1 xs = foldr max xs

-- rmin :: Ord a => [a] -> a
-- rmin xs = foldr min (last xs) (init xs)

-- rmin1 :: Ord a => [a] -> a
-- rmin1 xs = foldr min xs

--b
-- myfoldr1 :: (a -> a -> a) -> [a] -> a
-- myfoldr1 xs = foldr funq (last xs) (init xs)

-- myfoldl1 :: (a -> a -> a) -> [a] -> a
-- myfoldl1 xs = foldl funq (head xs) (tail xs)

--55
--a
add :: Int -> Int -> Int
add i 0 = i
add i j = succ (add i (pred j))

mult :: Int -> Int -> Int
mult i 0 = 0
mult i j = add i (pred j)

exp :: Int -> Int -> Int
exp i 0 = 1
exp i j = mult i (pred j)

--58
fact :: [Integer]
fact = 1:zipWith (*) fact (tail inteiros)
    where inteiros = 0:map (1+) inteiros

factn :: Int -> Integer
factn n = fact !! n

fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

fibn :: Int -> Integer
fibn n = fib !! n

--59
-- merge :: Ord a => [a] -> [a] -> [a]
-- merge l1 [] = l1
-- merge [] l2 = l2
-- merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
--                     | y < x = y:merge (x:xs) ys
--                     | x == y = x:merge xs ys
--
-- merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
-- merge xs ys zs = merge xs (merge ys zs)
--
-- hamming :: Integer
-- hamming = 1:merge3 (map (2*) hamming) (map (3*) hamming) (map (5*) hamming)

--60 ou como o 57
somas :: [Int] -> [Int]
somas xs = scanl (+) 0 xs

--57
somas' :: [Int] -> [Int]
somas' l = 0:zipWith (+) (somas' l) l                      --recursiva

--62, necessita 53
rotateInfinite :: [a] -> [[a]]
rotateInfinite xs = iterate shift xs

--64
dobros :: [Integer] -> [Integer]
dobros l = 1:zipWith (*) (dobros l) l

--65
elefantes :: Int -> IO()
elefantes n | n < 3 = return ()
            | otherwise = sequence_ [putStrLn(verso x) | x <- [2..n-1]]

verso :: Int -> String
verso n = "Se " ++ show n ++ " elefantes incomodam muita gente, \n" ++ show (n+1) ++ " elefantes incomodam muito mais!"

--66
wc :: String -> IO()
wc s = putStrLn s'
          where s' = show(length(lines s)) ++ "\t" ++
                     show(length(words s)) ++ "\t" ++
                     show(length s)

--67
strRev :: String -> String
strRev s = reverse s

--usado do exercicio 71 ao 78
data Arv a = Vazia | No a (Arv a) (Arv a)
  deriving Show

arv1 = No 15 (No 8 (No 5 (No 1 Vazia Vazia) (No 7 Vazia Vazia))
                   (No 10 (No 9 Vazia Vazia) (No 13 Vazia Vazia)))
             (No 20 (No 18 Vazia Vazia)
                    (No 25 (No 22 Vazia Vazia) (No 33 Vazia Vazia)))

--71
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esq dir) = sumArv esq + x + sumArv dir

--72
listarDesc :: Arv a -> [a]
listarDesc Vazia = []
listarDesc (No x esq dir) = listarDesc dir ++ [x] ++ listarDesc esq

--73
nivelArv :: Int -> Arv a -> [a]
nivelArv n Vazia = []
nivelArv 0 (No x esq dir) = [x]
nivelArv n (No x esq dir) = nivelArv (n-1) esq ++ nivelArv (n-1) dir

--75
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv f Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

--77
--a
maisEsq :: Arv a -> a
maisEsq (No x Vazia _) = x
maisEsq (No _ esq _) = maisEsq esq

maisDir :: Arv a -> a
maisDir (No x _ Vazia) = x
maisDir (No _ _ dir) = maisDir dir

--b
remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir) | x == y = dir
remover x (No y esq Vazia) | x == y = esq
remover x (No y esq dir) | x < y = No y (remover x esq) dir
                         | x > y = No y esq (remover x dir)
                         | x == y = let z = maisDir dir
                                    in No z esq (remover z dir)

--79
--a
data Shape = Circle Float | Rectangle Float Float
  deriving (Eq, Show)

--b
perimetro :: Shape -> Float
perimetro (Circle r) = 2 * pi * r
perimetro (Rectangle h w) = 2 * h + 2 * w

--80
--1
type Rect = ((Float,Float),(Float,Float))

--2
area :: Rect -> Float
area ((x1,y1),(x2,y2)) = (x2 - x1) * (y2 - y1)

--3
intersect :: Rect -> Rect -> Bool
intersect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) | y2 < y3 = False
                                              | y4 < y1 = False
                                              | x2 < x3 = False
                                              | x4 < x1 = False
                                              | otherwise = True

--81
--import Stack
parent :: String -> Bool
parent s = parentaux s empty

parentaux :: String -> Stack Char -> Bool
parentaux [] s = isEmpty s
parentaux (x:xs) s | x == '(' = parentaux xs (push ')' s)
                   | x == '[' = parentaux xs (push ']' s)
                   | x == '{' = parentaux xs (push '}' s)
                   | otherwise = not(isEmpty s) && x == top s && parentaux xs (pop s)

--82
--import Stack
-- a
calc :: Stack Float -> String -> Stack Float
calc s ys | ys == "*" = push((top(pop s)) * (top s)) (pop(pop s))
          | ys == "+" = push((top(pop s)) + (top s)) (pop(pop s))
          | ys == "-" = push((top(pop s)) - (top s)) (pop(pop s))
          | ys == "/" = push((top(pop s)) / (top s)) (pop(pop s))
          |otherwise = push (read ys) s

-- b
calcular :: String -> Float
calcular str = calcularAux (words str) empty

calcularAux :: [String] -> Stack Float -> Float
calcularAux [] s = top s
calcularAux (x:xs) s =  calcularAux xs (calc s x)
