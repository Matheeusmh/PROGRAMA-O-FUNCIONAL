filterDef :: (a -> Bool) -> [a] -> [a]
filterDef _ [] = []
filterDef f (x:xs) | f x = x : filterDef f xs
                   | otherwise = filterDef f xs

mapDef :: (a -> b) -> [a] -> [b]
mapDef _ [] = []
mapDef f (x:xs) = f x : mapDef f xs

zipWithDef :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDef f (x:xs) (y:ys) = f x y : (zipWithDef f xs ys)
zipWithDef _ _ _ = [] 

foldlDef :: (a -> b -> a) -> a -> [b] -> a
foldlDef _ z [] = z
foldlDef f z (x:xs) = foldlDef f (f z x) xs

foldrDef :: (a -> b -> b) -> b -> [a] -> b
foldrDef _ z [] = z 
foldrDef f z (x:xs) = f x (foldrDef f z xs)

foldl1Def :: (a -> a -> a) -> [a] -> a
foldl1Def f (x:xs) = foldl f x xs

foldr1Def :: (a -> a -> a) -> [a] -> a
foldr1Def _ [x] = x
foldr1Def f (x:xs) = f x (foldr1Def f xs)
