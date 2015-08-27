module Main where

import Data.List
import Data.Maybe

lookupTable :: [(Char, [String])]
lookupTable = [('I', ["ATT", "ATC", "ATA"]),
               ('L', ["CTT", "CTC", "CTA", "CTG", "TTA", "TTG"]),
               ('V', ["GTT", "GTC", "GTA", "GTG"]),
               ('F', ["TTT", "TTC"]),
               ('M', ["ATG"]),
               ('C', ["TGT", "TGC"]),
               ('A', ["GCT", "GCC", "GCA", "GCG"]),
               ('G', ["GGT", "GGC", "GGA", "GGG"]),
               ('P', ["CCT", "CCC", "CCA", "CCG"]),
               ('T', ["ACT", "ACC", "ACA", "ACG"]),
               ('S', ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"]),
               ('Y', ["TAT", "TAC"]),
               ('W', ["TGG"]),
               ('Q', ["CAA", "CAG"]),
               ('N', ["AAT", "AAC"]),
               ('H', ["CAT", "CAC"]),
               ('E', ["GAA", "GAG"]),
               ('D', ["GAT", "GAC"]),
               ('K', ["AAA", "AAG"]),
               ('R', ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"])]

buildMatches :: String -> [String]
buildMatches zs = foldr (\ele acc -> 
    [y++x | x <- acc, y <- (fromMaybe [""] $ lookup ele lookupTable)])
                  [""] zs

-- Possible Matches -> Genome -> Matches
encoder :: [String] -> String -> [String]
encoder [] _ = []
encoder patterns text = matches
    where matches = [x | x <- patterns, y <- chunks, x == y]
          chunks = map (take patternLength) sublists
          patternLength = (length . head) patterns
          sublists = (filter (\x -> length x >= patternLength) . tails) text

complement :: String -> String
complement = reverse . map complement'
    where
        complement' x
            | x == 'T' = 'A'
            | x == 'C' = 'G'
            | x == 'G' = 'C'
            | x == 'A' = 'T'
            | otherwise = undefined

parseContents :: String -> (String, String)
parseContents xs = let (genome:peptide:rest) = lines xs
    in (peptide, genome)

main :: IO ()
main = do
    contents <- getContents
    let (peptide, genome) = parseContents contents
        matches = buildMatches peptide
        first = encoder matches genome
        second = (map complement . encoder matches . complement) genome
    putStrLn . show $ intercalate " " (first ++ second)
