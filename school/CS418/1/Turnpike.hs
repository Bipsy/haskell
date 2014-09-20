import Data.List

generateDistances :: Int -> [Int] -> [Int]
generateDistances y = map (\x -> abs (y-x))

-- On isSubset x y determines if y is a subset of x
isSubset :: [Int] -> [Int] -> Bool
isSubset xs ys = null (ys \\ xs)

-- turnpike x where x is a sorted list (non-descending) of differences
turnpike :: [Int] -> [Int]
turnpike xs =
