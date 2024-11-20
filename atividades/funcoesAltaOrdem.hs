map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

zipWith :: (a->b->c) -> [a] -> [b] -> c
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWhith f xs ys

filter :: (a->bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) | f x = x : filter f xs
	        | otherwise = filter f xs

foldl :: (a-> b->a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x)  xs

foldr :: (a -> b -> a) -> a -> [b] -> a
foldr f z [] = z
foldr f z (x:xs) = f x (foldl f z xs)