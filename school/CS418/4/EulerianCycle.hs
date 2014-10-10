module Main where

import qualified Data.Map as Map
import Data.List
import qualified Data.List.Split as Split

main :: IO ()
main = do
    contents <- getContents
    let parsedContents = parseContents $ lines contents
        graph = buildGraph parsedContents
        firstPair = firstCycle graph
        cycle = eulerianCycle firstPair
        formattedResult = format cycle
    print formattedResult

eulerianCycle :: ([(Int,Int)], Map.Map Int [Int]) -> [(Int,Int)]
eulerianCycle (c,g) = 
    let seed = nextStart g
        in case seed of
            Nothing -> c
            Just val -> eulerianCycle $ takeAWalk val c g

parseContents :: [String] -> [(Int,[Int])]
parseContents = map parseContents'
    where parseContents' x = 
              let (a:_:as:_) = words x
                  bs = Split.splitOn "," as
                  in (read a, map read bs)

format :: [(Int,Int)] -> String
format cycle = 
    let format' (x,_) acc = show x : acc
        end = formatEnd $ last cycle
        formatEnd (a,b) = show a ++ "->" ++ show b
        in intercalate "->" . foldr format' [end] $ init cycle

buildGraph :: [(Int,[Int])] -> Map.Map Int [Int]
buildGraph = foldr (\(a,b) dict -> Map.insertWith' (++) a b dict) Map.empty

updateGraph :: Map.Map Int [Int] -> Int 
               -> ((Maybe Int,Maybe Int), Map.Map Int [Int])
updateGraph dict key = lookupAndUpdate key dict
    where lookupAndUpdate key dict = 
              let maybeUpdate (_,Nothing) dict = ((Nothing,Nothing), dict)
                  maybeUpdate (k,Just []) dict = 
                      ((Nothing,Nothing), Map.delete key dict)
                  maybeUpdate (k,Just val) dict = 
                      (((Just k), (Just (head val)))
                       , Map.insert k (tail val) dict)
                  in maybeUpdate (key, Map.lookup key dict) dict

firstCycle :: Map.Map Int [Int] -> ([(Int,Int)], Map.Map Int [Int])
firstCycle dict = 
    let seed = updateGraph dict (head $ Map.keys dict)
        firstCycle' ((Nothing,_),g) = ([],g)
        firstCycle' ((_,Nothing),g) = ([],g)
        firstCycle' ((Just k,Just v), g) = 
            let (cycle,newGraph) = (firstCycle' $ updateGraph g v)
                in (((k,v):cycle),newGraph)
        in firstCycle' seed

reorderCycle :: Int -> [(Int,Int)] -> [(Int,Int)]
reorderCycle k cycle = 
    let (Just index) = findIndex (\(a,b) -> a == k) cycle
        (first,last) = splitAt index cycle
        in last ++ first

nextStart :: Map.Map Int [Int] -> Maybe Int
nextStart graph = 
    let pred (_,[]) = False
        pred (_,_) = True
        edges = filter pred $ Map.toList graph
        in case edges of
            [] -> Nothing
            _ -> Just . fst $ head edges

takeAWalk :: Int -> [(Int,Int)] -> Map.Map Int [Int] 
             -> ([(Int,Int)], Map.Map Int [Int])
takeAWalk start cycle graph =
    let newCycle = reorderCycle start cycle
        takeAWalk' ((Nothing,_),g) = (newCycle,g)
        takeAWalk' ((_,Nothing),g) = (newCycle,g)
        takeAWalk' ((Just k,Just v), g) =
            let (newCycle',newGraph) = (takeAWalk' $ updateGraph g v)
                in (((k,v):newCycle'),newGraph)
        in takeAWalk' $ updateGraph graph start
