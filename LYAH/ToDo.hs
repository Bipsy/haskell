import System.Environment
import System.IO
import System.Directory
import Data.List

main :: IO ()
main = do
	(command:args) <- getArgs
	let action = lookup command dispatch
	case action of
		(Just act) -> act args
		Nothing	   -> return ()


dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("removed", remove),
			("view", view)]

add :: [String] -> IO ()
add (path:rest) = do
	withFile path AppendMode (\handle -> 
		hPutStr handle (unwords rest))
add _ 			= return ()

view :: [String] -> IO ()
view (path:_) 	= do
	withFile path ReadMode (\handle -> do
		contents <- hGetContents handle
		putStrLn contents)

view _			= return ()

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! (number)) todoTasks
        --orderedItems = orderItems newTodoItems  
    hPutStr tempHandle $ unlines newTodoItems 
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName

orderItems :: [String] -> [String]
orderItems = label . unlabel
    where 
	    unlabel [] 		= []
	    unlabel (x:xs) 	= (drop 3 x):(unlabel xs)
	    label = label' 1
		    where
			    label' _ [] 		= []
			    label' num (x:xs)	= ((show num) ++ ". " ++ x):(label' (num+1) xs)
                
