module Main where

import qualified Data.Map as Map
import Data.List
import Data.Maybe

main :: IO ()
main = do
    putStrLn "Hello, World"

lookupTable :: [(Char, [String])]
lookupTable = [('I', ["AUU", "AUC", "AUA"]),
               ('L', ["CUU", "CUC", "CUA", "CUG", "UUA", "UUG"]),
               ('V', ["GUU", "GUC", "GUA", "GUG"]),
               ('F', ["UUU", "UUC"]),
               ('M', ["AUG"]),
               ('C', ["UGU", "UGC"]),
               ('A', ["GCU", "GCC", "GCA", "GCG"]),
               ('G', ["GGU", "GGC", "GGA", "GGG"]),
               ('P', ["CCU", "CCC", "CCA", "CCG"]),
               ('T', ["ACU", "ACC", "ACA", "ACG"]),
               ('S', ["UCU", "UCC", "UCA", "UCG", "AGU", "AGC"]),
               ('Y', ["UAU", "UAC"]),
               ('W', ["UGG"]),
               ('Q', ["CAA", "CAG"]),
               ('N', ["AAU", "AAC"]),
               ('H', ["CAU", "CAC"]),
               ('E', ["GAA", "GAG"]),
               ('D', ["GAU", "GAC"]),
               ('K', ["AAA", "AAG"]),
               ('R', ["CGU", "CGC", "CGA", "CGG", "AGA", "AGG"])]

rlt :: [(String, Char)]
rlt = Map.toList $ foldOuter lookupTable
    where foldOuter = foldr (\(c, cs) table -> foldInner c cs table) Map.empty
          foldInner c cs table = 
            foldr (\ele acc -> Map.insert ele c acc) table cs 

translate :: String -> String
translate pattern = translated
    where translated = map (\codon -> fromMaybe '!' $ lookup codon rlt) codons
          codons = map (take 3) filtered
          filtered = snd . unzip $ filter (\(a,b) -> a `mod` 3 == 0) sublists
          sublists = zip [0..] . filter (\x -> length x >= 3) $ tails pattern
