-- a)



-- b)

orderTriple :: Ord a => (a,a,a) -> (a,a,a) --(Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (x,y,z) |y <= x && x <= z = (y,x,z)
                    |y <= z && z <= x = (y,z,x)
                    |z <= y && y <= x = (z,y,x)
                    |z <= x && x <= y = (z,x,y)
                    |x <= z && z <= y = (x,z,y)
                    |otherwise = (x,y,z)
