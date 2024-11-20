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
replica n v = foldn (v :) [] n

dupS :: String -> String
dupS [] = []
dupS xs = concat [(replica 2 x ) | x <- xs]

--map using foldr
--map' :: (a -> b) -> [a] -> [b]
--map' f  = foldr (\x acc -> f x : acc) []

pertence :: Eq a => a -> [a] -> Bool
pertence v xs = foldl (||) False (map (== v) xs)

remdups :: Eq a => [a] -> [a]
remdups [] = []
remdups (x:xs) = x : filter (/= x) (remdups xs)

todos :: (a -> Bool) -> [a] -> Bool
todos p xs = foldl (&&) True (map p xs)

positivos :: [Int] -> Bool
positivos xs = todos (>= 0) xs

algum :: (a -> Bool) -> [a] -> Bool
algum p xs = foldl (||) False (map p xs)

membro :: Eq a => a -> [a] -> Bool
membro v xs = foldl (||) False (map (== v) xs)

