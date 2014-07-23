import Data.Char

main :: IO ()
main = interact isPalindrome

isPalindrome :: String -> String
isPalindrome = unlines . map (\line -> 
	if (line == reverse line)
		then "Yes"
		else "No") . lines
