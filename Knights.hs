module Knights where

import Control.Monad

type Position = (Int,Int)

nextMoves :: Position -> [Position]
nextMoves (x,y) = do 
    (x',y') <- [(x-2,y+1), (x-1,y+2), 
              (x+1,y+2), (x+2,y+1),
              (x+1,y-2), (x+2,y-1),
              (x-1,y-1), (x-2,y-1)]
    guard (x' `elem` [1..8] && y' `elem` [1..8])
    return (x',y')

in3 :: Position -> [Position]
in3 pos = uniquePositions $ nextMoves pos >>= 
            uniquePositions . nextMoves >>= 
            uniquePositions . nextMoves

canReachIn3 :: Position -> Position -> Bool
canReachIn3 a b = b `elem` in3 a

uniquePositions :: [Position] -> [Position]
uniquePositions [] = []
uniquePositions (x:xs) 
    | x `elem` xs = uniquePositions xs
    | otherwise   = x : uniquePositions xs

numIn3 :: Position -> (Position, Int)
numIn3 pos = (pos, length $ in3 pos)

possibilities :: [(Position, Int)]
possibilities = do
    i <- [1..8]
    j <- [1..8]
    return $ numIn3 (i,j)

mostReachable :: [(Position, Int)] -> Maybe Position
mostReachable [] = Nothing
mostReachable list = Just ((fst . foldr (\ (pos, num) (pos', num') -> 
                        if (num > num') 
                            then (pos, num)
                            else (pos', num')) ((0,0), 0)) list)
