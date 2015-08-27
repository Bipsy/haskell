module Party where

import Employee
import Data.Monoid
import Data.Tree
import System.IO

main :: IO ()
main = do
    withFile "company.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let tree = read contents
        let results = maxFun tree
        putStrLn $ printGuestList results)

instance Monoid GuestList where
    mempty = (GL [] 0)
    mappend (GL list fun) (GL list' fun') = (GL (list <> list') (fun+fun'))

printGuestList :: GuestList -> String
printGuestList (GL list fun) = foldl 
    (\ acc ele -> acc ++ (empName ele) ++ "\n")
    ("Total Fun: " ++ show fun ++ "\n")
    list

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list score) = (GL (emp:list) (score+(empFun emp)))

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl gl' =
    if (gl > gl') 
        then gl
        else gl'

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = ((GL [emp] (empFun emp)), (GL [] 0))
nextLevel emp list = ((foldr with (GL [emp] (empFun emp)) list), 
                      (foldr without mempty list))
    where
        with ele acc = (snd ele) <> acc
        without (a,b) acc = (moreFun a b) <> acc

maxFun :: Tree Employee -> GuestList
maxFun tree = let (a,b) = treeFold nextLevel tree
            in moreFun a b
