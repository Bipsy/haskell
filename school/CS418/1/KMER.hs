module Main where 

import qualified Data.Map as Map
import System.IO

addEmUp :: String -> Map.Map String Int -> Map.Map String Int
addEmUp string map' = addEmUp' (Map.lookup string map') string map'
    where
        addEmUp' (Just val) string map' = Map.insert string (val+1) map'
        addEmUp' Nothing string map' = Map.insert string 1 map'

findKMER :: [String] -> Map.Map String Int
findKMER strings = foldr addEmUp Map.empty strings

scanString :: String -> Int -> Map.Map String Int
scanString [] _ = Map.empty
scanString string len = scanString' string len Map.empty
    where
        scanString' string@(x:xs) len map' =
            if (length string < len)
                then map'
                else scanString' xs len $ addEmUp (take len string) map'
                 
maxVal :: Map.Map String Int -> Int
maxVal map' = Map.fold (\ ele acc -> 
                if ele > acc 
                    then ele
                    else acc) 0 map'

reportResults :: Int -> Map.Map String Int -> (Int, [String])
reportResults val map' = (val, Map.keys $ Map.filter (== val) map')


main :: IO ()
main = do
    withFile "text.txt" ReadMode (\ handle -> do
        contents <- hGetContents handle
        let res = scanString contents 11
            val = maxVal res
        putStr $ show $ reportResults val res)
