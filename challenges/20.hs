removeAt :: [a] -> Int -> [a]
removeAt list num = take (num-1) list ++ drop num list 
