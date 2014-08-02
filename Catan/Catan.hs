module Main where

import Control.Applicative
import qualified Data.Map.Strict as Map

main :: IO ()
main = do 
    return ()


odds :: Map.Map Integer Integer
odds = counted possibilities
    where
        counted = foldr (\ el acc -> Map.insertWith (+) el 1 acc) Map.empty
        possibilities = (+) <$> [1..6] <*> [1..6]
       

oddsFinder :: [Integer] -> Maybe Double
oddsFinder = foldr (\ ele acc -> 
    let value = ((/36) <$> fromInteger <$> Map.lookup ele odds)
        in (*) <$> value <*> acc)
                   (Just 1) 
