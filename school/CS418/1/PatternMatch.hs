module Main where

import System.Environment
import System.IO
import Data.List

main :: IO ()
main = do
    args@(x:xs) <- getArgs
    (pattern, genome) <- extractData x
    let res = findMatches pattern genome
    mapM_ (putStr . (++ " " ) . show) res
    putStrLn ""
    return ()

findMatches :: String -> String -> [Int]
findMatches pattern genome = 
    (findIndices (pattern `isPrefixOf`) 
    . tails) genome
        
extractData :: String -> IO (String, String)
extractData filePath = do
    withFile filePath ReadMode (\ handle -> do
        contents <- hGetContents handle
        let lines' = lines contents
        if (length lines' > 1) 
            then do
                 putStrLn $ "Lines: " ++ (show $ length lines')
                 putStrLn $ "Pattern: " ++ head lines'
                 putStrLn $ "Genome: " ++ unlines (drop 1 lines')
                 return (head lines', unlines (drop 1 lines'))
            else return ("",""))
