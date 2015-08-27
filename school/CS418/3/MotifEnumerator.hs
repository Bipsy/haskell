module Main where

import qualified Data.Map as Map
import Data.List
import System.Environment

generate :: String -> Int -> Int -> Int -> String -> [String]
generate partial index d d' string =
    if (index == length string - 1)
        then [partial]
        else if (d >= d')
            then generate (partial++[(string !! (index+1))])
                          (index+1)
                          d
                          d'
                          string
            else generate (partial++"A")
                          (index+1)
                          (clash string index 'A' d)
                          d'
                          string
                 ++ generate (partial++"T")
                             (index+1)
                             (clash string index 'T' d)
                             d'
                             string
                 ++ generate (partial++"C")
                             (index+1)
                             (clash string index 'C' d)
                             d'
                             string
                 ++ generate (partial++"G")
                             (index+1)
                             (clash string index 'G' d)
                             d'
                             string
    where clash xs i c d = if (xs !! (index+1) == c)
            then d
            else (d+1)

generateKMER :: Int -> Int -> Map.Map String Int -> String -> Map.Map String Int
generateKMER k d map' gene = res
    where res = foldr (\ele acc -> Map.insertWith' (+) ele 1 acc) map' mutKmers
          mutKmers = foldr (union) [] $ map (generate "" (-1) 0 d) kmers
          kmers = map (take k) sublists
          sublists = filter (\x -> length x >= k) $ tails gene

motifEnumeration :: Int -> Int -> [String] -> [String]
motifEnumeration k d genes = Map.keys res
    where res = Map.filter (== numGenes) mappedKmers
          numGenes = length genes
          mappedKmers = foldr (\ele acc -> generateKMER k d acc ele) 
                              Map.empty 
                              genes

parseContents :: String -> [String]
parseContents = lines

main :: IO ()
main = do
    contents <- getContents
    args <- getArgs
    let genes = parseContents contents
        res = motifEnumeration (read (args !! 0)) (read (args !! 1)) genes
    putStrLn . show $ intercalate " " res
            
