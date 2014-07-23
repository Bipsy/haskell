slice :: [a] -> Int -> Int -> [a]
slice list first last = take (last-first+1) (drop (first-1) list)
