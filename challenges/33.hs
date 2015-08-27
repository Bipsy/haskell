euclid :: Int -> Int -> Int
euclid a b 
	| a < b = euclid b a
 	| otherwise = euclid' a b
		where
			euclid' _ 0 = 1
			euclid' a b 
				| a `mod` b == 0 	= b
				| otherwise 		= euclid' b (rem a b)

coPrime :: Int -> Int -> Bool
coPrime a b = if (euclid a b) == 1
				then True
				else False
