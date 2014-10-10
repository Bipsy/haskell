module Main where

import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- getContents
    let (k,text) = parseContents contents
        res = format . deBruijn $ kmers k text
    mapM_ putStrLn res

parseContents :: String -> (Int,String)
parseContents contents = let (k:text:rest) = lines contents
                             in (read k,text)

format :: Map.Map String [String] -> [String]
format = tail . foldr format' [] . Map.toList
    where format' (k,v) acc = (k ++ " -> " ++ intercalate "," v) : acc

kmers :: Int -> String -> [String]
kmers k xs = (map (take k) . filter (\x -> length x >= k) $ tails xs) 
          ++ [((tail . reverse . take k $ reverse xs) ++ ['T'])]

deBruijn :: [String] -> Map.Map String [String]
deBruijn = snd . foldl deBruijn' ("",Map.empty)
    where deBruijn' (last,dict) ele = 
              let prefix = init ele
                  in (prefix,Map.insertWith' (++) last [prefix] dict)


