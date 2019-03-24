triangulo :: Float -> Float -> Float -> Bool
triangulo a b c = (a + b > c) && (a + c > b) && (b + c > a)
triangulo a b c = if(a + b > c)
										then if(a + c > b)
											then if(b + c > a)
												then True
											else False
										else False
									else False

{-
triangulo a b c | (a + b > c) && (a + c > b) && (b + c > a) = True
								| otherwise = False
-}
