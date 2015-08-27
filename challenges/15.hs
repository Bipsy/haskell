duplicator :: [a] -> [a]
duplicator = foldr (\ ele acc -> (ele:ele:[]) ++ acc) []
