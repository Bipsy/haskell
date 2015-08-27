module Main where 

import qualified Data.Map as Map
import System.IO
import Data.List

scanString :: (Ord a) => [a] -> Int -> Map.Map [a] Int
scanString xs k = res
    where res = foldr (\ele acc -> Map.insertWith' (+) (take k ele) 1 acc) 
                      Map.empty 
                      sublists
          sublists = filter (\x -> length x >= k) (tails xs)
                 
maxVal :: Map.Map [a] Int -> Int
maxVal = Map.fold (\ ele acc -> if ele > acc then ele else acc) 0 

findKMER :: (Ord a) => [a] -> Int -> (Int, [[a]])
findKMER xs k = reportResults val kmers
    where kmers = scanString xs k
          val = maxVal kmers

reportResults :: Int -> Map.Map [a] Int -> (Int, [[a]])
reportResults val map' = (val, Map.keys $ Map.filter (== val) map')

parseContents :: String -> (String, Int)
parseContents string = 
    let (first:second:xs) = lines string
        in (first, read second)

main :: IO ()
main = do
    contents <- getContents
    let (xs, k) = parseContents contents
        res = findKMER xs k
    putStrLn $ show res
