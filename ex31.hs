multisoma :: Int -> Int -> Int
multisoma x 1 = x
multisoma x y = x + (multisoma x (y-1))
