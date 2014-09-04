

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
    deriving (Show)

type BreadCrumbs a = [Crumb a]

type Zipper a = (Tree a, BreadCrumbs a)

goLeft :: Zipper a -> Zipper a
goLeft (Node item l r, xs) = (l, LeftCrumb item r:xs)

goRight :: Zipper a -> Zipper a
goRight (Node item l r, xs) = (r, RightCrumb item l:xs)

goUp :: Zipper a -> Zipper a
goUp (l, LeftCrumb item r:xs) = (Node item l r, xs)
goUp (r, RightCrumb item l:xs) = (Node item l r, xs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node item l r, xs) = (Node (f item) l r, xs)
modify f (Empty, xs) = (Empty, xs) 

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, xs) = (t, xs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost focus = topMost $ goUp focus

extractTree :: Zipper a -> Tree a
extractTree (t, xs) = t

insert :: (Ord a) => a -> Tree a -> Tree a
insert item tree = insert' item (tree, [])
    where
        insert' item (Empty, xs) = 
            (extractTree . topMost . (attach (Node item Empty Empty))) 
                (Empty, xs)
        insert' item focus@(Node item' l r, xs)
            | item > item' = (insert' item . goRight) focus
            | item < item' = (insert' item . goLeft) focus
            | otherwise = (extractTree . topMost) focus

remove :: (Ord a) => a -> Tree a -> Tree a
remove item tree = remove' item (tree, [])
    where
        remove' item focus@(Empty, _) = (extractTree . topMost) focus
        remove' item focus@(Node item' l r, xs)
            | item > item' = (remove' item . goRight) focus
            | item < item' = (remove' item . goLeft) focus
            | otherwise = 
                let newTree = removeNode (Node item' l r)
                    in (extractTree . topMost) (newTree, xs)

removeNode :: (Ord a) => Tree a -> Tree a
removeNode Empty = Empty
removeNode (Node _ Empty Empty) = Empty
removeNode (Node _ l Empty) = l
removeNode (Node _ Empty r) = r
removeNode (Node _ l r) = let key = minKey r
                              r' = remove key r
                              in (Node key l r')

minKey :: Tree a -> a
minKey (Node key Empty _) = key
minKey (Node _ l _) = minKey l

(-:) :: a -> (a -> b) -> b
x -: y = y x
