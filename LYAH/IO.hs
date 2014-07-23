import Control.Monad
import Data.Char

main :: IO ()
main = do
	line <- getLine
	if null line
		then return ()
		else do
			print line
			main

printStr :: String -> IO ()
printStr [] 	= do
					putChar '\n'
					return ()
printStr (x:xs) = do
					putChar x
					printStr xs

printTilSpace :: IO ()
printTilSpace = do
	c <- getChar
	if (c == ' ')
		then return ()
		else do
			putChar c
			printTilSpace

printTilSpaceWhen :: IO ()
printTilSpaceWhen = do
	c <- getChar
	when (c /= ' ') $ do
		putChar c
		printTilSpaceWhen


printThreeTimes :: IO ()
printThreeTimes = do
	lines <- sequence [getLine, getLine, getLine]
	print lines

printArrayWeird :: (Show a) => [a] -> IO ()
printArrayWeird = mapM_ print

hangMan :: IO ()
hangMan = do
	putStrLn "Do you want to play a game?"
	response <- getLine
	case map toLower response of
		"yes" -> do
			putStrLn "Alright lets play"
			putStrLn "I am going to pick a number between 1 and 10"
			putStrLn "If you guess that number in less than 3 tries you win otherwise...I win."
			game 5 0
				where
					game num 2 = do
						putStrLn "What is your first guess?"
						line <- getLine
						if (read line == num)
							then do
								putStrLn $ "You guessed it: " ++ (show num)
							else do
								putStrLn $ "Wrong, the answer was: " ++ (show num)
								putStrLn "I win"
					game num guesses = do
						putStrLn "What is your first guess?"
						line <- getLine
						if (read line == num)
							then do
								putStrLn $ "You guessed it " ++ show num
								putStrLn "You win"
							else do
								putStrLn "Wrong, guess again"
								game num (guesses+1)
		"no" -> do
			putStrLn "Bummer dude, Lets play another time then"
		_ -> 	do
			putStrLn "I didn't understand you (yes/no)"
