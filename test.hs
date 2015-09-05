remove :: (Ord a) => a ->[a] -> [a]
remove _ [] = []
remove a (x:xs)
    | a == x = remove a xs
    | otherwise = x : remove a xs