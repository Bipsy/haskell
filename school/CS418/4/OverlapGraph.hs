module Main where

import qualified Data.Map as Map

overlapGraph :: [String] -> [(String, String)]
overlapGraph kmers = res
    where res = foldr extractMatches [] kmers
          dict = foldr insertStrings Map.empty kmers
          insertStrings ele acc = Map.insertWith' (++) (tail ele) [ele] acc
          extractMatches ele acc = 
              let list = [(a,ele) | a <- (Map.findWithDefault [] front dict)]  
                  front = init ele
                  in list ++ acc 

format :: [(String, String)] -> [String]
format = map (\(a,b) -> a ++ " -> " ++ b)   

main :: IO ()
main = do
    contents <- getContents
    let pairs = format . overlapGraph $ lines contents
    mapM_ putStrLn pairs
        
