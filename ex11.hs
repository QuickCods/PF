safetail :: [a] -> [a]
safetail [] =  []
safetail (x:xs) = xs

safetail2 :: [a] -> [a]
safetail2 l = if null l then [] else tail l