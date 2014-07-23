import System.IO

main :: IO ()
main = do
	withFile "stuff.txt" ReadMode (\handle -> do
		contents <- hGetContents handle 
		putStr contents)
