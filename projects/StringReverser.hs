main :: IO ()
main = do
    putStrLn "Enter a string, then hit return"
    line <- getLine
    putStrLn $ reverse line
