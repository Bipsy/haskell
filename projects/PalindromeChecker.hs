import Control.Monad
import Data.Char

main :: IO ()
main = do
    putStrLn $ "Enter a string, if it a palindrome True will be output. " ++
             "If it empty then the program will exit."
    let main' = do
        line <- getLine
        when (length line /= 0) $ do
            let fixedLine = map toLower line
            if (fixedLine == reverse fixedLine)
                then putStrLn "True"
                else putStrLn "False"
            main'
        in main'
