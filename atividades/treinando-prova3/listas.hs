import Data.List (delete)

--permuta
permuta :: Eq a => [a] -> [[a]]
permuta [] = [[]]
permuta xs = [x:ys | x <- xs, ys <- permuta (delete x xs)]

--replica
foldn :: (a -> a) -> a -> Int -> a
foldn f e 0 = e
foldn f e n = f (foldn f e (n-1))

replica :: Int -> a -> [a] 
replica n x = foldn (x :) [] n 

dupS :: String -> String
dupS xs = concat [(replica 2 x) | x <- xs]


pertence :: Eq a => a -> [a] -> Bool
pertence v xs = foldr (||) False (map (== v) xs)

remdups :: Eq a => [a] -> [a]
remdups [] = []
remdups (x:xs) = x : filter (/= x) (remdups xs)

todos :: (a -> Bool) -> [a] -> Bool
todos f xs = foldr (&&) True (map f xs)

positivos :: [Int] -> Bool
positivos xs = todos even xs

algum :: (a -> Bool) -> [a] -> Bool
algum p xs = foldr (||) False (map p xs)

membro :: Eq a => a -> [a] -> Bool
membro v xs = algum (== v) xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs

foldl' :: (a->b->a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs 

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl' f x xs

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [x] = x
foldr1' f (x:xs) =  f x (foldr1' f xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)
zipWith' _ _ _ = []

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith'' _ _ _ = []