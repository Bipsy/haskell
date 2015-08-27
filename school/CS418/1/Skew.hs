main :: IO ()
main = do
    contents <- getContents
    putStrLn $ show $ skew contents

skew :: String -> (Int, Int, Int, [Int])
skew = foldl skew' (0, 0, 0, [])

skew' (low, curr, index, xs) ele
        | curr < low = if (ele == 'C') 
                               then (curr, (curr-1), (index+1), [index])
                               else if (ele == 'G')
                                   then (curr, (curr+1), (index+1), [index])
                                   else (curr, curr, (index+1), [index])
        | curr == low = if (ele == 'C')
                                then (curr, (curr-1), (index+1), index:xs)
                                else if (ele == 'G')
                                    then (curr, (curr+1), (index+1), index:xs)
                                    else (curr, curr, (index+1), index:xs)
        | otherwise = if (ele == 'C')
                          then (low, (curr-1), (index+1), xs)
                          else if (ele == 'G')
                              then (low, (curr+1), (index+1), xs)
                              else (low, curr, (index+1), xs)

skewer :: String -> (Int, [Int])
skewer xs = foldl (\(prev, vals) ele -> 
    if (ele == 'C')
        then ((prev-1), prev:vals)
        else if (ele == 'G')
            then ((prev+1), prev:vals)
            else (prev, prev:vals)) (0, []) xs
