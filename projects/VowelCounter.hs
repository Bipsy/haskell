main :: IO ()
main = do
    contents <- getContents
    putStr $ vowelCounter contents

vowelCounter :: String -> String
vowelCounter line = 
    let a = (length $ filter (\x -> x == 'a') line, 'a')
        e = (length $ filter (\x -> x == 'e') line, 'e')
        i = (length $ filter (\x -> x == 'i') line, 'i')
        o = (length $ filter (\x -> x == 'o') line, 'o')
        u = (length $ filter (\x -> x == 'u') line, 'u')
        in foldl (\ acc (x,y) -> acc ++ show y ++ ":" ++ show x ++ "\n") 
                 [] [a, e, i, o, u]
    
