import Data.List

patternMatch :: String -> String -> Int -> [Int]
patternMatch pattern genome d = res
    where res = findIndices (approx pattern d) chunks
          chunks = map (take (length pattern)) sublists
          sublists = filter (\x -> length x >= length pattern) (tails genome)
          approx pattern d chunk = if (foldr (\(a,b) acc -> 
                                            if (a == b) 
                                                then acc
                                                else acc+1)
                                         0
                                         (zip pattern chunk) > d) then False
                                                                 else True 
                                         
main :: IO ()
main = do
    contents <- getContents
    let (pattern, genome, d) = parseContents contents
        res = patternMatch pattern genome d
    putStrLn $ intercalate " " $ map show res

parseContents :: String -> (String, String, Int)
parseContents xs = let (pattern:genome:count:rest) = lines xs
    in (pattern, genome, read count)
