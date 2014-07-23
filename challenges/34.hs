toTientPhi :: Int -> Int
toTientPhi num 
	| num < 4 	= 1
	| otherwise = toTientPhi' num (num-1)
		where
			toTientPhi' _ 1 = 1
			toTientPhi' a b = if (coPrime a b)
								then 1 + toTientPhi' a (b-1)
								else toTientPhi' a (b-1)

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
