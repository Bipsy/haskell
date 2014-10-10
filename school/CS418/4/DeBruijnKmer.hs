module Main where

import qualified Data.Map as Map
import Data.List

main :: IO ()
main = do 
    contents <- getContents
    let res = deBruijn $ lines contents
        formattedResult = format res
    mapM_ putStrLn formattedResult

format :: Map.Map String [String] -> [String]
format dict = 
    let pairs = Map.toList dict
        formattedPairs = map format' pairs
        format' (a,bs) = a ++ " -> " ++ intercalate "," bs
        in formattedPairs

deBruijn :: [String] -> Map.Map String [String]
deBruijn = foldl (\acc ele -> Map.insertWith' (++) (init ele) [(tail ele)] acc)
                 Map.empty
