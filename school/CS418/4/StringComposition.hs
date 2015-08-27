module Main where

import Data.List

main :: IO ()
main = do
    contents <- getContents
    let (k, text) = parseContents contents
        res = composition k text
    mapM_ putStrLn res

composition :: Int -> String -> [String]
composition k = sort . map (take k) . filter (\x -> length x >= k) . tails 

parseContents :: String -> (Int, String)
parseContents xs = let (k:text:rest) = lines xs
                       in (read k, text)
