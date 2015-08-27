main :: IO ()
main = do
    contents <- getContents
    let numWords = (length . words) contents
    putStrLn $ "The number of words is " ++ show numWords
