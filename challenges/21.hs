insertAt :: a -> [a] -> Int -> [a]
insertAt ele list index = if ((index < 1) || (index > length list))
							then list
							else take (index-1) list ++ [ele] ++ drop index list
