bits :: Int -> [[Bool]]
bits  0 = [[]]
bits n = [x:l | x <-[False, True], l <- bits (n-1)]
