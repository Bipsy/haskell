module Main where

main :: IO ()
main = do
    contents <- getLine
    putStrLn ""
    putStrLn $ complement contents

complement :: String -> String
complement = reverse . map complement'
    where
        complement' x 
            | x == 'T' = 'A'
            | x == 'C' = 'G'
            | x == 'G' = 'C'
            | x == 'A' = 'T'
            | otherwise = undefined
            
