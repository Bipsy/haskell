module Main where

import Data.List
import Data.Function
import Control.Arrow
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- getContents
    let (genome, (k, l, t)) = parseContents contents
        res = intercalate " " . removeDuplicates $ clumpFinding genome k l t
    putStrLn res

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

clumpFinding :: (Ord a) => [a] -> Int -> Int -> Int -> [[a]]
clumpFinding xs k l t = foldr (++) [] clumps
    where clumps = map (\x -> snd x) matches
          matches = filter (\ x -> fst x == t) kmers
          kmers = map (`findKMER` k) sublists
          sublists = filter (\x -> length x >= l) (tails xs)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = foldr (\ ele seen -> if (ele `elem` seen)
                                            then seen
                                            else ele : seen)
                         []

parseContents :: String -> (String, (Int, Int, Int))
parseContents string = 
    let (firstline:secondline:rest) = lines string
        genome = firstline
        (k:l:t:xs) = map read $ words secondline
        in (genome, (k,l,t))
