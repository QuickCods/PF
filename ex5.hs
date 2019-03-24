--a)

mylast :: [a] -> a
mylast k = head(reverse k)

mylast' k = head(drop (length-1) k)

--b)

myinit :: [a] -> [a]
myinit l = reverse(tail(reverse l))

myinit' l = take (length-1) l
