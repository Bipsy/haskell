dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list index = dropEvery' list index 1
	where
		dropEvery' [] _ _ = []
		dropEvery' (x:xs) index num = 
			if (num == index) 
				then (dropEvery' xs index 1)
				else (x:(dropEvery' xs index (num+1)))
