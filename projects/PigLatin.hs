import System.Exit
import Control.Monad

main :: IO ()
main = do
    putStrLn "Enter a word to have it converted to pig latin."
    putStrLn "If you enter an empty word the program will be executed."
    putStrLn "If you enter multiple words (separated by spaces) then only the first word will be translated"
    forever $ do
        line <- getLine
        if (null line)
            then exitSuccess
            else putStrLn $ pigLatin line

pigLatin :: String -> String
pigLatin word = drop 1 word ++ "-" ++ take 1 word ++ "ay"
