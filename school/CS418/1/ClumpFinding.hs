module Main where

import Data.List
import Data.Function
import Control.Arrow

main :: IO ()
main = return () >>= \_ -> getLine >>= \line -> putStrLn line

mostFrequentKmers :: (Ord a) => [a] -> Int -> [[a]]
mostFrequentKmers xs k = map snd $ head . groupBy ((==) `on` fst) $ sortedFreq
  where sortedFreq  = reverse $ sortBy (compare `on` fst) frequencies
        frequencies = map (length &&& head) kmers
        kmers = group . sort $ foldr (\x acc -> take k x : acc) [] sublists
        sublists = filter (\x -> length x >= k) (tails xs)
