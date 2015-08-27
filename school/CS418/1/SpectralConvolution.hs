module Main where

import Data.List

main :: IO ()
main = do
    contents <- getContents
    let spectrum = parseContents contents
        res = convolution spectrum
        printable = intercalate " " $ map show res
    putStrLn printable

parseContents :: String -> [Int]
parseContents = map read . words

convolution :: [Int] -> [Int]
convolution xs = concat . sortBy listLength $ group sorted
    where sorted = sort . snd . foldr (convolute) ([head xs], []) $ tail xs 
          convolute ele (as, bs) = (ele:as, differences ele as ++ bs)
          differences n = map (\x -> abs (n - x))
          listLength a b = if length a > length b then GT else LT

