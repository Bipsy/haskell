import Data.List
import Data.Maybe

kmer :: Int -> String -> [String]
kmer k xs = res
    where res = map (take k) sublists
          sublists = filter (\x -> length x >= k) $ tails xs

analyze :: [[Double]] -> String
analyze xs = res
    where res = toString transposed
          transposed = transpose xs
          toString = map (\x -> 
            case (fromJust $ findIndex (== (maximum x)) x) of 
                0 -> 'A'
                1 -> 'C'
                2 -> 'G'
                3 -> 'T')

