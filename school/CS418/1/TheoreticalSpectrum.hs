

import Data.Maybe
import Data.List

lookupTable :: [(Char, Int)]
lookupTable = [('G', 57), ('A', 71), ('S', 87), ('P', 97),
               ('V', 99), ('T', 101), ('C', 103), ('I', 113),
               ('L', 113), ('N', 114), ('D', 115), ('K', 128),
               ('Q', 128), ('E', 129), ('M', 131), ('H', 137),
               ('F', 147), ('R', 156), ('Y', 156), ('W', 186)]

convertToNums :: String -> [Int]
convertToNums = map (\x -> fromMaybe 0 . lookup x $ lookupTable)

shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft (x:xs) = xs ++ [x]

contigSublists :: [a] -> [[a]]
contigSublists xs = res ++ [xs]
    where res = foldr (++) [] groups
          groups = map taker cyclicLists
          len = length xs
          cyclicLists = take len . iterate shiftLeft $ xs
          taker ys = init . tail $ inits ys

parseContents :: String -> String
parseContents xs = let (text:_) = lines xs
                       in text

main :: IO ()
main = do 
    contents <- getContents
    let text = parseContents contents
        res = (0:) . sort . map sum . contigSublists $ convertToNums text
    putStrLn . intercalate " " $ map show res
