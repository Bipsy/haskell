module Calculator where

import Data.List
import Data.Char

parse :: String -> [String]
parse = words

eval :: [String] -> [String]
eval = foldl eval' []
    where 
        eval' [] ele = [ele]
        eval' acc@(x:y:ys) ele = if (all isDigit ele) 
            then (ele:acc)
            else case ele of
                "+" -> (combine x y (+)):ys
                "-" -> (combine x y (-)):ys                
                "*" -> (combine x y (*)):ys
                _   -> error "Improper Expression"
        eval' acc@(x:xs) ele = if (all isDigit ele)
            then (ele:acc) 
            else error "Improper Expression"

combine :: String -> String -> (Int -> Int -> Int) -> String
combine a b op =  show $ (read a) `op` (read b)  
