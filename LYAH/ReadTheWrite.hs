import System.IO

main :: IO ()
main = withFile "stuff.txt" ReadMode (\rHandle -> 
	withFile "more-stuff.txt" WriteMode (\wHandle -> do
		contents <- hGetContents rHandle
		hPutStr wHandle contents))
