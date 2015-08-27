module Main where

import Data.List
import qualified Data.Map as Map

generateMatches :: String -> Int -> String
generateMatches xs d = generateMatches' xs d 0 Map.empty
    where generateMatches' [] _ _ res = res
          generateMatches' (x:xs) d e res = 
            | e < d = 
            | otherwise = res  
