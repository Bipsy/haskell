module Main where

import Data.List
import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector
import Control.Arrow

type Motifs = (Matrix.Matrix Int, [String])

main :: IO ()
main = do
    contents <- getContents
    let (k,strings) = parseContents contents
        res = greedyMotif k strings
    putStrLn $ show res

parseContents :: String -> (Int,[String])
parseContents xs = (3, tail $ lines xs)

updateProfile :: Matrix.Matrix Int -> String -> Matrix.Matrix Int
updateProfile matrix string = foldr updateMatrix matrix (zip [1..] string)
    where updateMatrix (col,c) acc = updateElem (+) (getRow c) col acc 1

updateElem :: (a -> a -> a) -> Int -> Int -> Matrix.Matrix a -> a 
              -> Matrix.Matrix a
updateElem f i j matrix new = updatedMatrix
    where old = Matrix.getElem i j matrix
          updatedMatrix = Matrix.setElem (f new old) (i,j) matrix

getRow c = case c of
    'A' -> 1
    'C' -> 2
    'G' -> 3
    'T' -> 4
    _   -> error "Not a valid Character"

countMatches :: Matrix.Matrix Int -> String -> Int
countMatches matrix string =
    foldr (\(col,c) acc -> Matrix.getElem (getRow c) col matrix + acc)
          0
          (zip [1..] string)

mostProbableKmer :: Matrix.Matrix Int -> Int -> String -> String
mostProbableKmer matrix k text = snd mostProbableKmer'
    where kmers' = kmers k text
          mostProbableKmer' = foldr matcher (0,"") kmers'
          matcher ele acc@(old,_) = let new = countMatches matrix ele
              in if new > old then (new,ele) else acc

score :: Matrix.Matrix Int -> Int
score matrix = score'
    where maxCol = foldr1 (\ele acc -> if ele > acc then ele else acc)
          sumCol = foldr1 (+)
          score' = sum . map (\(a,b) -> a-b) 
                   . map (sumCol &&& maxCol) 
                   . transpose $ Matrix.toLists matrix

firstKmers :: Int -> [String] -> [String]
firstKmers k = map (take k)

kmers :: Int -> String -> [String]
kmers k = map (take k) . filter (\x -> length x >= k) . tails

greedyMotif :: Int -> [String] -> [String]
greedyMotif k strings = snd bestMotifs
    where firstMotifs = firstKmers k strings
          firstProfile = foldr (\ele acc -> updateProfile acc ele)
              (Matrix.zero 4 k)
              firstMotifs
          firstStringKmers = kmers k $ head strings
          bestMotifs = foldl outer (firstProfile,firstMotifs) firstStringKmers
          -- ele is Motif, acc is (profile,motifs)
          outer old@(o_prof,_) kmer = let new@(n_prof,_) = buildMotifs kmer
              in if score n_prof < score o_prof then new else old
          -- inner should produce a (profile,motif) tuple
          buildMotifs kmer = let 
              start = updateProfile (Matrix.zero 4 k) kmer
              motif = [kmer]
              in foldl innerLoop (start,motif) $ tail strings
          -- acc is a (profile,motifs) tuple and ele is string of DNA
          innerLoop (profile,motifs) strand = 
              let mostProbable = mostProbableKmer profile k strand
                  updatedMotifs = mostProbable : motifs
                  updatedProfile = updateProfile profile mostProbable
                  in (updatedProfile,updatedMotifs)

                            
