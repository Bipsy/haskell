module Main where

import qualified Data.Map as Map
import Data.List

scanString :: (Ord a) => [a] -> Int -> Map.Map [a] [Int]
scanString xs k = res
    where res = foldr (\(xs,i) acc -> Map.insertWith' (++) (take k xs) [i] acc)
                      Map.empty 
                      withIndeces
          withIndeces = zip sublists [0..]
          sublists = filter (\x -> length x >= k) (tails xs)

closeEnough :: Map.Map [a] [Int] -> Int -> Int -> Map.Map [a] [Int]
closeEnough map' l t = Map.filter (closeEnough' l t) map'
    where closeEnough' l t val = foldr (||) False (answers val)
          answers xs = map (\xs -> if (last xs - head xs <= l) then True else False) (chunks xs)
          chunks xs = map (take t) (sublists xs)
          sublists xs = filter (\x -> length x >= t) (tails xs)

parseContents :: String -> (String, (Int, Int, Int))
parseContents string =
    let (firstline:secondline:rest) = lines string
        genome = firstline
        (k:l:t:xs) = map read $ words secondline
        in (genome, (k,l,t))

clumpFinder :: (Ord a) => [a] -> Int -> Int -> Int -> [[a]]
clumpFinder xs k l t = Map.keys $ closeEnough (scanString xs k) l t

main :: IO ()
main = do
    contents <- getContents
    let (genome, (k, l, t)) = parseContents contents
        res = show $ clumpFinder genome k l t
    putStrLn res
