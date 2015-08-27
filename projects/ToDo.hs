module Main where

import System.Environment
import System.IO
import System.Directory

dispatch :: [(String, ([String] -> IO ()))]
dispatch = [("add", add),
            ("remove", remove),
            ("view", view)]

usage :: String
usage = "Usage: ToDo <command> <path-to-file> <options>"

main :: IO ()
main = do
    args <- getArgs
    let (command, options) = extractArgs args
        result = lookup command dispatch
    case result of
        (Just action) -> action options
        Nothing -> error "command not recognized"

extractArgs :: [String] -> (String, [String])
extractArgs [] = error usage
extractArgs (x:xs) = (x,xs)

add :: [String] -> IO ()
add [] = return ()
add (fileName:item:_) = do
    withFile fileName AppendMode (\handle -> do
        hPutStrLn handle item)
add _ = error usage

remove :: [String] -> IO ()
remove [] = error usage
remove (fileName:s_index:_) = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "foo.ext"
    contents <- hGetContents handle 
    let items = lines contents
        indexedItems = zip [1..] items
        i_index = read s_index
        zippedItems = filter (\ (index, _) -> index /= i_index) indexedItems
        cleanItems = foldr (\(index,item) acc -> item:acc) [] zippedItems
    hPutStr tempHandle (unlines cleanItems)
    renameFile tempName fileName
    hClose tempHandle
    hClose handle
remove _ = return ()

view :: [String] -> IO ()
view [] = error usage
view (fileName:_) = do
    withFile fileName ReadMode (\handle -> do
        contents <- hGetContents handle
        let items = lines contents
            indexedItems = zipWith (\ a b -> show a ++ ". " ++ b) [1..] items
        putStr $ unlines indexedItems)
